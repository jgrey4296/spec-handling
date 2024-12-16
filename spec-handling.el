;;; spec-handling.el -*- lexical-binding: t; no-byte-compile: t;  -*-
(require 'dash)
(require 'cl-lib)
(require 'benchmark)
(require 'helpful)

(defvar sh-hook nil "Hooks registered to run each time spec handlers are re-applied")

(defvar sh-sources (make-hash-table :test 'equal) "Records where handlers are defined and used")

(defvar sh-docs    (make-hash-table :test 'equal) "Contains plists of handler documentation")

(defconst sh-gensym-plist '(:table "spec-table"
                            :apply "reapply-specs-fn"
                            :feature "spec-feature"
                            :mode-hook "spec-hook-init-fn"
                            :set "spec-set"
                            :add "spec-add-delayed-fn"
                            )
  "conversion table for kwd -> name parts"
  )

(defconst sh-symbol-separator "-")

(defconst sh-doc-str
  "%s (Macro-Generated spec handler)
Source: %s
Args: %s

%s
")

(defun sh-unquote! (val)
  "Generalized unquoting function
Does not recursively unquote.
"
  (declare (pure t) (side-effect-free t))
  (if (and (consp val) (memq (car val) '(quote function)))
      (cadr val)
    val
    )
  )

(defun sh--pop-kwds-from-body (body)
  "Functions with &key and &rest include the keys in the body,
(and need &allow-other-keys.
This strips the kwds and their values off from the body
"
  (while (and body (keywordp (car body))) (pop body) (pop body))
  body
  )

(defun sh--add-source (id file &optional form doc structure target optional)
  "register an id of spec handler and where it is used"
  (unless (or (null file)
              (--some (and (eq (car it) (or form :source)) (eq (cadr it) file))
                      (gethash id sh-sources nil)))
    (push (list (or form :use) file) (gethash id sh-sources))
    )
  (when (or doc structure target optional)
    (puthash id `(:doc ,doc :structure ,structure :target ,target :optional ,optional) sh-docs))
  (gethash id sh-sources nil)
)

(defun sh--gensym (&rest names)
  " make a newly interned symbol from the provided name strings/symbols/keywords,
separated by 'spec-handling-symbol-separator', looking up keywords in 'spec-handling-gensym-plist' "
  (intern (string-join (mapcar (lambda (x)
                                 (cond
                                  ((keywordp x)
                                   (plist-get sh-gensym-plist x))
                                  ((symbolp x)
                                   (symbol-name x))
                                  (t
                                   x))
                                 )
                               names)
                       sh-symbol-separator
                       )
          )
  )

(defun sh-first-run ()
  "Provides a feature on the first application of spec handlers only."
  (message "Reapply Spec Hook Firing")
  (provide 'sh-first-run)
  )

(defun sh-cleanup-after-provide (sym)
  " Remove closures from 'after-load-alist' that are now unneeded, because 'sym' has been provided.
this stops them being re-run repeatedly
"
  (interactive "x")
  (setq after-load-alist (--remove (equal (car it) sym) after-load-alist))
  nil
  )

(defun sh--gen-new-hook (id body)
  " generate the loop body for a new hook

 "
  `(do
    (cl-loop for mode in (ensure-list key)
             do
             (let ((fn-name (sh--gensym (quote ,id) mode :mode-hook)))
               (fset fn-name
                     (-partial (lambda (val)
                                 ,@body
                                 )
                               val)
                     )
               (add-hook (intern (format "%s-hook" mode)) fn-name)
               )
             )
    )
  )

;;;###autoload
(defun run-spec-handlers ()
  " Run spec handlers defined with spec-handling-new! and spec-handling-add! "
  (interactive)
  (message "Spec Handlers Ran in: %s seconds"
           (benchmark-elapse
             (run-hooks 'sh-hook)
             )
           )
  )

;;;###autoload
(cl-defmacro spec-handling-new! (id doc &rest body
                         &key (target nil) (sorted nil)
                         (dedup nil) (loop 'do) (override nil)
                         (struct nil) (optional nil) &allow-other-keys)
  " Register a Spec Handler Type.
Each handler type is uniquely id'd, and describes how registered specs are applied.

:struct is for documenting the handler and its expected spec structure.
:loop is in (do | collect | append ), describing the type of cl-loop the handler uses.

:target names a variable it modifies
:sorted can sort generated values before applying to the target
:dedup  can remove duplicates

The body of the handler is run for each registered spec.
Binding (key . (val)) from the spec table of the handler.

return the generated feature name of this spec type
 "
  (let* ((table-name   (sh--gensym id :table))
         (reapply-name (sh--gensym id :apply))
         (feature-name (sh--gensym id :feature))
         (fname (macroexp-file-name))
         (vals (make-symbol "vals"))
         (loop-kw (sh-unquote! loop))
         (sort-fn (pcase sorted
                    ('nil nil)
                    ('t '(lambda (x y) (< (car x) (car y))))
                    (_ sorted)))
         (unless-check (pcase override
                         ('t '(nil))
                         (_  `((featurep (quote ,feature-name))))
                         ))
         (clean-body (sh--pop-kwds-from-body body))
         (docstring (format sh-doc-str id fname (list :sorted sorted :loop loop)
                            (s-concat doc (when struct (format "\n\nExpected Struct: %s" struct)))))
         )
    ;; Remove keywords and their values from body:
    (cl-assert (or override (not (fboundp reapply-name))) "Handler is already defined: %s" reapply-name)
    (cl-assert clean-body t "Body of a spec handling definition can not be empty")
    (cl-assert (memq loop-kw '(collect append do hook)))
    (cl-assert (not (eq loop-kw 'hook)) "Use spec-handling-new-hook! instead")
    (cl-assert (or target (eq loop-kw 'do)) t "Must have a target if loop isnt a 'do")
    (cl-assert (not (and target (eq loop-kw 'do))) t "Can't have a target if loop is 'do")
    (cl-assert (not (and sorted (eq loop-kw 'do))) t "Sorting a 'do or 'hook loop doesn't make sense")
    ;; The macro's returned code:
     `(unless ,@unless-check
        (sh--add-source (quote ,id) ,fname :definition
                        ,doc ,struct ,(if target (quote target) nil) ,optional)
        (defvar ,table-name (make-hash-table :test 'equal),(format "Macro generated hash-table to store specs for %s" id))
        (fset (function ,reapply-name)
              (lambda (&optional dry)
                ,docstring
                (interactive)
                (let ((,vals (cl-loop for key being the hash-keys of ,table-name
                                      using (hash-values val)
                                      ,loop-kw
                                      ,@clean-body
                                      )))
                  ,@(when sorted
                     `((setq ,vals (mapcar #'cdr (sort ,vals ,sort-fn))))
                     )
                  ,@(when dedup
                      `((setq ,vals (cl-remove-duplicates (-reject #'null ,vals))))
                      )
                  ,@(when target
                      `((unless dry
                          (setq ,target ,vals)))
                      )
                  ,vals
                  )
                )
              )
        (add-hook (quote sh-hook) (function ,reapply-name))
        (provide (quote ,feature-name))
        (sh-cleanup-after-provide (quote ,feature-name))
        (quote ,feature-name)
        )
     )
  )

;;;###autoload
(cl-defmacro spec-handling-new-hook! (id doc &rest body
                              &key (struct nil) (optional nil) (override nil)
                              &allow-other-keys)
  " Register a new hook spec "
  (let* ((table-name   (sh--gensym id :table))
         (reapply-name (sh--gensym id :apply))
         (feature-name (sh--gensym id :feature))
         (fname (macroexp-file-name))
         (vals  (make-symbol "vals"))
         (unless-check (pcase override
                         ('nil `((-contains? sh-hook (function ,reapply-name))))
                         ('t (nil))))
         (clean-body (sh--pop-kwds-from-body body))
         (docstring (apply #'s-concat doc (when struct (list "\n\nExpected Struct: " struct))))
         )
    (cl-assert clean-body t "Body of a spec handling hook instance can not be empty")
    (cl-assert (or override (not (fboundp reapply-name))) "Hook Handler is already defined: %s" reapply-name)
    `(unless ,@unless-check
       (sh--add-source (quote ,id) ,fname :hooks-definition
                       ,doc ,struct (quote ,target) ,optional)
       (defvar ,table-name (make-hash-table :test 'equal)
         ,(format "Macro generated hash-table to store specs for %s" id))
       (fset (function ,reapply-name)

             (lambda (&optional dry)
               ,docstring
               ,(format sh-doc-str id fname sorted loop)
               (interactive)
               (cl-loop for key being the hash-keys of ,table-name
                        using (hash-values val)
                        ;; loop over keys/modes and create fns, adding to %mode-hook
                        ,@(sh--gen-new-hook id clean-body)
                        )
               )
             )
       (add-hook (quote sh-hook) (function ,reapply-name))
       (provide (quote ,feature-name))
       (sh-cleanup-after-provide (quote ,feature-name))
       (quote ,feature-name)
       )
    )
  )

;;;###autoload
(defmacro spec-handling-setq! (id priority &rest vals)
  " generate a setq hook "
  (let ((set-name (sh--gensym id :set))
        (fname (macroexp-file-name)))
    (cl-assert (fboundp set-name) "Tried to re-define: %s" set-name)
    `(progn
       (sh--add-source (quote ,id) ,fname :setting)
       (fset (function ,set-name) (lambda () (setq ,@vals)))
       (add-hook 'sh-hook (function ,set-name) ,priority)
      )
    )
  )

;;;###autoload
(cl-defmacro spec-handling-add! (id &rest rules &key (override nil) (extend nil) &allow-other-keys)
  " Add an instance of a spec handler, that was defined with spec-handling-new!

Rules are a list of values conforming to the handlers :struct definition.
eg: (spechandling-add! someHandler '(blah :bloo val :blee val))

"
  (let* ((fname   (macroexp-file-name))
         (val     (make-symbol "val"))
         (tempvar (make-symbol "curr"))
         (table-name   (sh--gensym id :table))
         (feature-name (sh--gensym id :feature))
         (add-fn-name  (sh--gensym id fname :add))
         (clean-rules (sh--pop-kwds-from-body rules))
         (form (pcase (sh-unquote! form)
                 ('override :override)
                 ('extend :extension)
                 (_ :addition)))
         )
    `(with-eval-after-load (quote ,feature-name)
       (sh--add-source (quote ,id) ,fname ,form)
       (cl-loop for ,val in (list ,@rules)
                for redefine = (null (gethash (car ,val) ,table-name nil))
                if (and redefine (not (or ,override extend))) do
                (message "Spec Handling Add: Attempt to override Spec: %s - %s - %s - %s"
                         (quote ,id) (car ,val) (gethash (car ,val) ,table-name) ,fname)
                else
                if ,extend do
                (puthash (car ,val) (append
                                     (gethash (car ,val) ,table-name nil)
                                     (cdr ,val))
                         ,table-name)
                else do
                (puthash (car ,val) (cdr ,val) ,table-name)
                )
       )
    )
  )

;;;###autoload
(defmacro spec-handling-clear! (id)
  " Clear a spec handler's registered instances,
and return the feature name that represents it
 "
  (let ((table-name (sh--gensym id :table))
        (reapply-name (sh--gensym id :apply))
        (feature-name (sh--gensym id :feature))
        )
    `(progn
       (when (boundp (quote ,table-name)) (clrhash ,table-name) (unintern (quote ,table-name) nil))
       (remove-hook 'sh-hook (function ,reapply-name))
       (unintern (function ,reapply-name) nil)
       (sh-cleanup-after-provide (quote ,feature-name))
       (setq features (remove (quote ,feature-name) features))
       (quote ,feature-name)
       )
    )
  )

;;;###autoload
(defun spec-handling-report ()
  " Generate a report on registered spec handlers,
and instances of those handlers.
"
  (interactive)
  (let ((temp-buffer-window-show-hook '(org-mode))
        (unique-files (make-hash-table :test 'equal))
        )
    (with-temp-buffer-window "*Spec-Report*" #'display-buffer-same-window nil
      (princ (format "* (%s) Registered Specs --------------------\n\n"
                     (length (hash-table-keys sh-sources))))
      (cl-loop for key being the hash-keys of sh-sources
               using (hash-values vals)
               do
               (mapcar (-compose (-rpartial #'puthash t unique-files) #'cadr) vals)
               (let ((defs (--select (-contains? '(:definition :hooks-definition) (car it)) vals))
                     (details-plist (gethash key sh-docs))
                     (adds (--select (equal :addition (car it)) vals))
                     (exts (--select (equal :extension (car it)) vals))
                     (sets (--select (equal :setting (car it)) vals))
                     )
                 (princ (format "** SPEC: %s\n" key))

                 (if defs
                     (dolist (def defs) (princ (format "*** Defined in: [[%s]]\n" (cadr def))))
                   (princ "*** SPEC NOT DEFINED ------------------------------\n"))
                 (princ (string-join (sh--build-description key details-plist "***") "\n"))

                 (if (null adds)
                     (princ "\n*** No Additions Defined\n")
                   (princ "\n*** Additions Defined in: \n")
                   (dolist (add adds) (princ (format "[[%s]]\n" (cadr add)))))
                 (when exts
                   (dolist (ext exts) (princ (format "**** Extended in [[%s]]\n" (cadr ext)))))
                 (when sets
                   (princ "\n*** Settings Defined in: \n")
                   (dolist (set sets) (princ (format "[[%s]]\n" (cadr set)))))
                 (princ "\n\n")
                 )
               )
      (princ "* File List:\n")
      (dolist (file (sort (hash-table-keys unique-files) #'string-lessp))
        (princ (format "%s\n" file)))
      )
    )
  )

;;;###autoload
(defun spec-handling-describe ()
  "Describe a specific spec handler"
  (interactive)
  (let* ((chosen (completing-read "Which Handler? " (hash-table-keys sh-docs)))
        (details-plist (gethash (intern chosen) sh-docs))
        )
    ;; TODO build a h
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (princ (format "Spec Handler: %s\n" chosen))
        (princ (string-join (sh--build-description chosen details-plist) "\n"))
        )
      )
    )
  )

(defun sh--build-description (chosen details-plist &optional leader)
  "Create a list of strings describing a spec handler"
  (list
   (format "%s Target Variable: %s" (or leader "----") (or (plist-get details-plist :target) "None"))

   ""
   (format "%s Spec Documentation:" (or leader "----"))
   ""
   (-if-let (doc (plist-get details-plist :doc))
       (format "%s" doc)
     "No Defined Documentation"
     )

   ""
   (format "%s Spec Structure:" (or leader "----"))
   ""
   (-if-let (struct (plist-get details-plist :structure))
       (helpful--pretty-print struct)
     "No Defined Structure"
     )
   )
  )

(provide 'spec-handling)

;; company
;; eval-handler
;; Local Variables:
;; read-symbol-shorthands: (
;; ("sh-" . "spec-handling-")
;; )
;; End:
