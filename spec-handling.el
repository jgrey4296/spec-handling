;;; spec-handling.el -*- lexical-binding: t; no-byte-compile: t;  -*-
(eval-when-compile
  (require 'dash)
  (require 'cl-lib)
  (require 'benchmark)
  (require 'helpful)
  )

(defvar sh-hook nil                               "Hooks registered to run each time spec handlers are re-applied")

(defvar sh-sources (make-hash-table :test 'equal) "Records where handlers are defined and used")

(defconst sh-gensym-plist '(:table                "speckler-table"
                            :apply                "speckler-apply-fn"
                            :feature              "speckler-feature"
                            :mode-hook            "speckler-hook-init-fn"
                            :set                  "speckler-set"
                            :add                  "speckler-add-delayed-fn"
                            )
  "conversion table for kwd -> name parts"
  )

(defconst sh-symbol-separator "-")

(defconst sh-doc-str
  "%s (Macro-Generated spec handler)
Source: %s
Args: %s

%s"
)

(cl-defstruct (sh-record)
  "Records Information about a declared handler or spec"
  (id        nil :type 'symbol)
  (file      nil :type 'string)
  (type      nil :type 'list)
  (doc       nil :type 'string)
  (struct    nil :type 'list)
  (target    nil :type 'symbol)
  (specs     nil :list 'list)
  (setqs     nil :list 'list)
  )

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

(defun sh-pop-kwds-from-body (body)
  "Functions with &key and &rest include the keys in the body,
  (and need &allow-other-keys.
         This strips the kwds and their values off from the body
         "
  (while (and body (keywordp (car body))) (pop body) (pop body))
  body
  )

(defun sh-add-source (id file type &rest plist)
  "register an id of spec handler and where it is used
         id is the handler id,
         type is a symbol of (handler hook spec setq)

         "
  (pcase (cons type (gethash id sh-sources nil))
    ((or `(handler . nil) `(hook . nil))
     ;; new handler, nothing registered
     (puthash id (apply #'make-spec-handling-record :id id :type type :file file plist)
              sh-sources))
    ((and (or `(handler . ,x) `(hook . ,x)) (guard (eq 'forward (sh-record-type x))))
     ;; forward dec exists, create the proper one, carrying specs over
     (puthash id (apply #'make-spec-handling-record :id id :file file :type type :specs (sh-record-specs x) :setqs (sh-record-setqs x) plist)
              sh-sources))
    (`(spec . nil)
     ;; spec without a handler. create a forward dec handler, add the spec
     (puthash id (make-spec-handling-record :id id
                                 :file nil
                                 :type 'forward
                                 :specs (list (make-spec-handling-record :id id
                                                              :file file
                                                              :type type)))
              sh-sources))
    (`(setq . nil)
     ;; setq without a handler. create a forward dec handler, add the spec
     (puthash id (make-spec-handling-record :id id
                                            :file nil
                                            :type 'forward
                                            :setqs (list (make-spec-handling-record :id id
                                                                                    :file file
                                                                                    :type type)))
              sh-sources))
    (`(spec . ,x)
     ;; theres already a handler
     (push (make-spec-handling-record :id id :file file :type type) (sh-record-specs x)))
    (`(setq . ,x)
     (push (make-spec-handling-record :id id :file file :type type) (sh-record-setqs x)))
    (x ;; other
     (display-warning 'speckler (format "Unknown source registration event: %s" x))
     )
    )
  )

(defun sh-gensym (&rest names)
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
                               (-reject #'null names))
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

(defun sh-gen-new-hook (key val id body)
  " generate the loop body for a new hook
Creates a partial function  bound to 'val'
and adds it to 'id'-hook

 "
  `(cl-loop for mode in (ensure-list ,key)
            for fn-name = (sh-gensym (quote ,id) mode :mode-hook)
            do
            (fset fn-name (-partial (lambda (val) ,@body) ,val))
            if (s-suffix? "-hook" (format "%s" mode))
            do      (add-hook mode fn-name)
            else do (add-hook (intern (format "%s-hook" mode)) fn-name)
            )
)

;;;###autoload (defalias 'speckler-go! #'spec-handling-run-handlers)
;;;###autoload (autoload 'spec-handling-run-handlings "spec-handling" nil t)
(defun sh-run-handlers ()
  " Run spec handlers defined with spec-handling-new! and spec-handling-add! "
  (interactive)
  (message "Spec Handlers Ran in: %s seconds"
           (benchmark-elapse
             (run-hooks 'sh-hook)
             )
           )
  )

;;;###autoload (defalias 'speckler-new! #'spec-handling-new-handler)
;;;###autoload (autoload 'spec-handling-new-handler "spec-handling" nil nil t)
(cl-defmacro sh-new-handler (id (key val) doc
                                &rest body
                                &key (target nil) (sorted nil) (setup nil)
                                (dedup nil) (loop do) (override nil) (example nil)
                                (struct nil) &allow-other-keys)
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
  (declare (indent defun))
  (let* ((table-name   (sh-gensym id :table))
         (apply-fn-name (sh-gensym id :apply))
         (feature-name (sh-gensym id :feature))
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
         (clean-body (sh-pop-kwds-from-body body))
         (docstring (format sh-doc-str id fname (list :sorted sorted :loop loop)
                            (s-concat doc (when struct (format "\n\nExpected Struct: %s" struct)))))
         )
    ;; Remove keywords and their values from body:
    (cl-assert clean-body t (format "Body of a spec handling definition can not be empty : %s" fname))
    (cl-assert (not (eq loop 'hook)) t (format "Use spec-handling-new-hook! instead : %s" fname))
    (cl-assert (memq loop-kw '(collect append do)) t (format "Use a :loop of do | collect | append, not '%s' : %s" loop-kw fname))
    (cl-assert (or target (eq loop-kw 'do)) t (format "Must have a target if loop isnt a 'do : %s : %s" fname loop-kw))
    (cl-assert (not (and target (eq loop-kw 'do))) t (format "Can't have a target if loop is 'do : %s : %s : %s" loop fname target))
    (cl-assert (not (and sorted (eq loop-kw 'do))) t (format "Sorting a 'do or 'hook loop doesn't make sense : %s" fname))
    ;; The macro's returned code:
     `(unless ,@unless-check
        (cl-assert (or ,override (not (fboundp (function ,apply-fn-name)))) t
                   ,(format "Handler is already defined: %s:%s" fname apply-fn-name))
        (sh-add-source (quote ,id) ,fname 'handler :doc ,doc :struct ,struct :target (quote ,target))
        (defvar ,table-name (make-hash-table :test 'equal),(format "Macro generated hash-table to store specs for %s" id))
        (fset (function ,apply-fn-name)
              (lambda (&optional dry)
                ,docstring
                (interactive)
                ,setup
                (let ((,vals (cl-loop for ,key being the hash-keys of ,table-name
                                      using (hash-values ,val)
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
        (add-hook (quote sh-hook) (function ,apply-fn-name))
        (provide (quote ,feature-name))
        (sh-cleanup-after-provide (quote ,feature-name))
        (quote ,feature-name)
        )
     )
  )

;;;###autoload (defalias 'speckler-new-hook! #'spec-handling-new-hook)
;;;###autoload (autoload 'spec-handling-new-hook "spec-handling" nil nil t)
(cl-defmacro sh-new-hook (id (key val) doc
                             &rest body
                             &key (struct nil) (optional nil) (override nil) (example nil)
                             &allow-other-keys)
  " Register a new hook handler.
(key val) are the names of the bound params available while creating hooks for a spec.
Each Spec registered added to the  `key'-hook
"
  (declare (indent defun))
  (let* ((table-name    (sh-gensym id :table))
         (apply-fn-name (sh-gensym id :apply))
         (feature-name  (sh-gensym id :feature))
         (fname (macroexp-file-name))
         (vals  (make-symbol "vals"))
         (unless-check (pcase override
                         ('nil `((-contains? sh-hook (function ,apply-fn-name))))
                         ('t (nil))))
         (clean-body (sh-pop-kwds-from-body body))
         (docstring (format sh-doc-str id fname (list :hook t)
                            (s-concat doc (when struct (format "\n\nExpected Struct: %s" struct)))))
         )
    (cl-assert clean-body t "Body of a spec handling hook instance can not be empty")
    `(unless ,@unless-check
       (cl-assert (or ,override (not (fboundp (function ,apply-fn-name))))  t
                  (format "Hook Handler is already defined: %s" apply-fn-name))
       (sh-add-source (quote ,id) ,fname 'hook :doc ,doc :struct ,struct)
       (defvar ,table-name (make-hash-table :test 'equal)
         ,(format "Macro generated hash-table to store specs for %s" id))
       (fset (function ,apply-fn-name)
             (lambda (&optional dry)
               ,docstring
               (interactive)
               (cl-loop for ,key being the hash-keys of ,table-name
                        using (hash-values ,val)
                        ;; loop over keys/modes and create fns, adding to %mode-hook
                        do
                        ,(sh-gen-new-hook key val id clean-body)
                        )
               )
             )
       (add-hook (quote sh-hook) (function ,apply-fn-name))
       (provide (quote ,feature-name))
       (sh-cleanup-after-provide (quote ,feature-name))
       (quote ,feature-name)
       )
    )
  )

;;;###autoload (defalias 'speckler-setq! #'spec-handling-register-setq)
;;;###autoload (autoload 'spec-handling-register-setq "spec-handling" nil nil t)
(cl-defmacro sh-register-setq (id (&optional mode mode-priority)
                                  &rest vals
                                  &key (priority 50) override
                                  &allow-other-keys)
  " generate a setq hook
use :priority int to set the priority the set hook is added at

if a mode is provided, the hook is added to mode-hook when speckler-go! is called
mode-priority allows mode-specific priority hook control
 "
  (declare (indent defun))
  (let* ((set-name (sh-gensym id :set))
         (set-mode-name (sh-gensym id mode :set))
         (fname (macroexp-file-name))
         (clean-vals (sh-pop-kwds-from-body vals))
         (hook-body (if mode
                        `(add-hook ,(intern (format "%s-hook" mode))
                          (function ,set-mode-name) ,(or mode-priority 50))
                      `(setq ,@clean-vals)))
         (mode-hook (when mode
                      `(fset (function ,set-mode-name)
                        (lambda ()
                          ,(format "%s specific speckler-setq hook" mode)
                          (setq-local ,@clean-vals)))))
         (override-warn (unless override
                          `(when (fboundp (function ,set-name))
                            (display-warning 'speckler ,(format "Tried to re-define setq: %s in %s" set-name fname)))))
         )
         `(progn
            ,override-warn
            (sh-add-source (quote ,id) ,fname 'setq)
            ,mode-hook
            (fset (function ,set-name) (lambda ()
                                         ,(format "Speckler hook for applying %s values " id)
                                         ,hook-body))
            (add-hook 'sh-hook (function ,set-name) ,priority)
            )
         )
  )

;;;###autoload (defalias 'speckler-add! #'spec-handling-add-spec)
;;;###autoload (autoload 'spec-handling-add-spec "spec-handling" nil nil t)
(cl-defmacro sh-add-spec (id () &rest rules &key (override nil) (extend nil) &allow-other-keys)
  " Add an instance of a spec handler, that was defined with spec-handling-new!

Rules are a list of values conforming to the handlers :struct definition.
eg: (spechandling-add! someHandler '(blah :bloo val :blee val))

"
  (declare (indent defun))
  (let* ((fname   (macroexp-file-name))
         (val     (make-symbol "val"))
         (tempvar (make-symbol "curr"))
         (table-name   (sh-gensym id :table))
         (feature-name (sh-gensym id :feature))
         (clean-rules (sh-pop-kwds-from-body rules))
         (form (list :override override :extension extend))
         )
    `(with-eval-after-load (quote ,feature-name)
       (sh-add-source (quote ,id) ,fname 'spec)
       (cl-loop for ,val in (list ,@clean-rules)
                for redefine = (gethash (car ,val) ,table-name nil)
                if (and redefine ,(not (or override extend))) do
                (display-warning 'speckler
                                 (format "Attempt to override: %s - %s - %s"
                                         (quote ,id) (car ,val) ,fname))
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

;;;###autoload (defalias 'speckler-clear! #'spec-handling-clear-registry)
;;;###autoload (autoload 'spec-handling-clear-registry "spec-handling" nil t)
(defun sh-clear-registry (id)
  " Clear a spec handler's registered instances,
         and return the feature name that represents it
 "
  (interactive (list (ivy-read "Handler To Clear: " (hash-table-keys sh-sources))))
  (let ((table-name (sh-gensym id :table))
        (apply-fn-name (sh-gensym id :apply))
        (feature-name (sh-gensym id :feature))
        )
    (when (boundp table-name)
      (message "Removing Handler Table: %s" table-name)
      (clrhash (eval table-name))
      (unintern (symbol-name table-name)))

    (when (fboundp apply-fn-name)
      (message "Removing Application Hook: %s" apply-fn-name)
      (remove-hook 'sh-hook apply-fn-name)
      (unintern (symbol-name apply-fn-name))
      )

    (message "Removing Feature: %s" feature-name)
    (sh-cleanup-after-provide feature-name)
    (setq features (remove feature-name features))

    (message "Removing Source: %s" id)
    (remhash (intern id) sh-sources)
    )
  )

;;;###autoload (defalias 'speckler-report! 'spec-handling-report)
;;;###autoload (autoload 'spec-handling-report "spec-handling" nil t)
(defun sh-report ()
  " Generate a report on registered spec handlers,
         and instances of those handlers.
         "
  (interactive)
  (let ((temp-buffer-window-show-hook '(org-mode))
        (unique-files (make-hash-table :test 'equal))
        )
    (with-help-window "*Speckle-Report*"
      (princ (format "* (%s) Registered Handlers--------------------\n\n"
                     (length (hash-table-keys sh-sources))))
      (cl-loop for key being the hash-keys of sh-sources
               using (hash-values record)
               do
               (puthash (sh-record-file record) t unique-files)
               (mapcar (-compose (-rpartial #'puthash t unique-files) #'sh-record-file) (sh-record-specs record))
               (princ (string-join (sh--build-description key record "**") "\n"))
               (princ "\n\n")
               )
      (princ (format  "* (%s) File List:\n" (length (hash-table-keys unique-files))))
      (dolist (file (sort (hash-table-keys unique-files) #'string-lessp))
        (when file (princ (format "[[%s][%s]]\n" file file))))
      )
    )
  )

;;;###autoload (defalias 'speckler-describe! 'spec-handling-describe)
;;;###autoload (autoload 'spec-handling-describe "spec-handling" nil t)
(defun sh-describe ()
  "Describe a specific spec handler"
  (interactive)
  (let* ((chosen (completing-read "Which Handler? " (hash-table-keys sh-sources)))
        (record (gethash (intern chosen) sh-sources))
        )
    (with-help-window (help-buffer)
        (princ (string-join (sh--build-description chosen record "*") "\n")))
      )
    )

(defun sh--build-description (chosen record &optional leader)
  "Create a list of strings describing a spec handler"
  (let* ((table-name    (sh-gensym chosen :table))
         (apply-fn-name (sh-gensym chosen :apply))
         (feature-name  (sh-gensym chosen :feature))
         (leader (or leader "**"))
         (subheading (format "*%s" leader))
         ;; (set-name      (sh-gensym chosen :set))
         )
    (append
     ;; Core
     (list
      (format "%s Handler: %s (%s)" leader chosen (length (sh-record-specs record)))
      (format "%s" (sh-record-doc record))
      ""
      (if-let (file (sh-record-file record))
          (format "%s File: [[%s]]" subheading file)
        (format "%s File: NOT DEFINED" subheading))
      ""
      (format "%s Handler Type: %s" subheading (sh-record-type record))
      ""
      (when (sh-record-target record)
        (format "%s Target Variable: `%s'\n"
                subheading
                (or (sh-record-target record) "None")))
      (format "%s Internals: " subheading)
      (format "- Table Name: `%s'" table-name)
      (format "- Apply function: `%s'" apply-fn-name)
      (format "- Feature: `%s'" feature-name)
      ""
      )
     ;; Structure
     (-if-let (struct (sh-record-struct record))
         (list (format "%s Structure:" subheading)
               (helpful--pretty-print struct)
               "")
       (list (format "%s No Defined Structure" subheading)
             "")
       )
     ;; Specs
     (if (null (sh-record-specs record))
         (list (format "%s No Specs Defined" subheading) "")
       (append (list (format "%s (%s) Specs Defined in:" subheading (length (sh-record-specs record))))
               (cl-loop for file in (sort (mapcar #'sh-record-file (sh-record-specs record)) #'string-lessp)
                        collect (format "[[%s][%s]]" file (f-base (f-parent file))))
               )
       )
     )
    )
  )

;; public aliases
(defvaralias 'speckler-hook 'spec-handling-hook)

(provide 'spec-handling)

;; company
;; eval-handler
;; Local Variables:
;; read-symbol-shorthands: (
;; ("sh-" . "spec-handling-")
;; ("make-sh-" . "make-spec-handling-")
;; )
;; End:
