;;; spec-handling.el -*- lexical-binding: t; no-byte-compile: t;  -*-
(require 'dash)
(require 'cl-lib)
(require 'benchmark)
(require 'helpful)

(defvar sh-hook nil)

(defvar sh-feature-set nil)

(defvar sh-types (make-hash-table :test 'equal) "Records where handlers are defined and used")

(defvar sh-docs (make-hash-table :test 'equal) "Contains plists of handler documentation")

(defconst sh-gensym-plist '(:table "spec-table"
                            :apply "reapply-specs-fn"
                            :feature "spec-feature"
                            :mode-hook "spec-hook-init-fn"
                            :set "spec-set"
                            :add "spec-add-delayed-fn"
                            ))

(defconst sh-symbol-separator "-")

(defconst sh-doc-str "Macro-Generated spec application fn for: %s\n from: %s\nArgs: (Sorted %s) (loop kw: %s)")

(defun sh-unquote! (val)
  (if (and (consp val) (memq (car val) '(quote function)))
      (cadr val)
    val
    )
  )

(defun sh--add-type (type file &optional form doc structure target optional)
  "register a type of spec handler and where it is used"
  (unless (or (null file)
              (--some (and (eq (car it) (or form :source)) (eq (cadr it) file))
                      (gethash type sh-types nil)))
    (push (list (or form :use) file) (gethash type sh-types))
    )
  (when (or doc structure target optional)
    (puthash type `(:doc ,doc :structure ,structure :target ,target :optional ,optional) sh-docs))
  (gethash type sh-types nil)
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

(defun sh--new-hook (type body)
  " generate the loop body for a new hook "
  `(do
    (cl-loop for mode in (ensure-list key)
             do
             (let ((fn-name (sh--gensym (quote ,type) mode :mode-hook)))
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
(cl-defmacro sh-new! (type target &rest body
                                      &key (sorted nil) (rmdups nil) (loop 'do) (form 'basic)
                                      (doc nil) (struct nil) (optional nil) &allow-other-keys)
  " Simplifies Spec application and definition
body is run for each (key . (vals)) of the spec-table and sets the value of target

if target is 'do, the specs are applied in the body and don't target a variable
TODO: add spec format docstring

return the generated feature name of this spec type

 "
  (let* ((table-name (sh--gensym type :table))
         (reapply-name (sh--gensym type :apply))
         (feature-name (sh--gensym type :feature))
         (fname (macroexp-file-name))
         (vals (make-symbol "vals"))
         (loop-kw (sh-unquote! loop))
         (sort-fn (pcase sorted
                    ('nil nil)
                    ('t '(lambda (x y) (< (car x) (car y))))
                    (_ sorted)))
         (unless-check (pcase loop-kw
                         ((guard (eq (sh-unquote! form) 'override)) `(nil))
                         ('hook `((-contains? sh-hook (function ,reapply-name))))
                         (_     `((featurep (quote ,feature-name))))
                         )
                       )
         )
    ;; Remove keywords and their values from body:
    (while (keywordp (car body)) (pop body) (pop body))
    (cl-assert body t "Body of a spec handling definition can not be empty")
    (cl-assert (memq loop-kw '(collect append do hook)))
    (cl-assert (or target (memq loop-kw '(do hook))) t "Must have a target if loop isnt a 'do or 'hook")
    (cl-assert (not (and target (memq loop-kw '(do hook)))) t "Can't have a target if loop is a 'do or 'hook")
    (cl-assert (not (and sorted (memq loop-kw '(do hook)))) t "Sorting a 'do or 'hook loop doesn't make sense")
    ;; The macro's returned code:
     `(unless ,@unless-check
        (sh--add-type (quote ,type) ,fname (pcase ,loop
                                                        ('hook :hooks-definition)
                                                        (_ :definition)
                                                        )
                                 ,doc ,struct (quote ,target) ,optional)
        (defvar ,table-name (make-hash-table :test 'equal),(format "Macro generated hash-table to store specs for %s" type ))
        (fset (function ,reapply-name)
              (lambda (&optional dry)
                ,(format sh-doc-str type fname sorted loop)
                (interactive)
                (let ((,vals (cl-loop for key being the hash-keys of ,table-name
                                      using (hash-values val)
                                      ,@(pcase loop-kw
                                          ('hook (sh--new-hook type body))
                                          (_ `(,loop-kw ,@body))
                                          )
                                      )))
                  ,@(when sorted
                     `((setq ,vals (mapcar #'cdr (sort ,vals ,sort-fn))))
                     )
                  ,@(when rmdups
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
(defmacro sh-setq! (type priority &rest vals)
  " generate a setq hook "
  (let ((set-name (sh--gensym type :set))
        (fname (macroexp-file-name)))
    `(progn
       (sh--add-type (quote ,type) ,fname :setting)
       (fset (function ,set-name) (lambda () (setq ,@vals)))
       (add-hook 'sh-hook (function ,set-name) ,priority)
      )
    )
  )

;;;###autoload
(cl-defmacro sh-add! (type &rest rules &key (form 'basic) &allow-other-keys)
  (let* ((fname (macroexp-file-name))
         (val (make-symbol "val"))
         (tempvar (make-symbol "curr"))
         (table-name (sh--gensym type :table))
         (feature-name (sh--gensym type :feature))
         (add-fn-name (sh--gensym type fname :add))
         (add-code (pcase (sh-unquote! form)
                     ('basic
                      `(if (null (gethash (car ,val) ,table-name nil))
                           do
                         (puthash (car ,val) (cdr ,val) ,table-name)
                         else do (message "Spec Handling Add: Attempt to override Spec: %s - %s - %s - %s" (quote ,type) (car ,val) (gethash (car ,val) ,table-name) ,fname)))
                     ('override `(do (puthash (car ,val) (cdr ,val) ,table-name)))
                     ('extend `(do (let ((,tempvar (gethash (car ,val) ,table-name nil)))
                                     (puthash (car ,val)
                                              (append ,tempvar (cdr ,val))
                                              ,table-name))))
                      )
                     )
         )
    (while (keywordp (car rules )) (pop rules) (pop rules))
    `(with-eval-after-load (quote ,feature-name)
       (sh--add-type (quote ,type) ,fname ,(pcase (sh-unquote! form)
                                             ('override :override)
                                             ('extend :extension)
                                             (_ :addition)))
       (cl-loop for ,val in (list ,@rules)
                ,@add-code
                )
       )
    )
  )

;;;###autoload
(defmacro sh-clear! (type)
  (let ((table-name (sh--gensym type :table))
        (reapply-name (sh--gensym type :apply))
        (feature-name (sh--gensym type :feature))
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
(defun sh-report ()
  (interactive)
  (let ((temp-buffer-window-show-hook '(org-mode))
        (unique-files (make-hash-table :test 'equal))
        )
    (with-temp-buffer-window "*Spec-Report*" #'display-buffer-same-window nil
      (princ (format "* (%s) Registered Specs --------------------\n\n" (length (hash-table-keys sh-types))))
      (cl-loop for key being the hash-keys of sh-types
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
(defun sh-describe ()
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
