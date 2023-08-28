# Spec-handling

Modes kept fucking up my personal preferences, so I wrote some macros to declaratively 
specify hooks, variables etc that i can re-run as a single hook.

Provides:
- `run-spec-handlers` : for when you want to get your specs reset to what you registered
- `spec-handling-new!` : register a new type of spec which is called when you run the handlers.
- `spec-handling-add!` : add a new spec, ready to re-use.
- `spec-handling-setq!` : used like setq, but is triggered in `run-spec-handlers`
- `spec-handling-report` : a command to produce an org file, telling you where new spec types are declared, and where implementations are defined.


As an Example:

``` emacs-lisp

;; Register a spec called `fold`, which targets `evil-fold-list`
;; It will be sorted, and works using a cl-loop `collect` on added values.
(spec-handling-new! fold evil-fold-list :sorted t :loop 'collect
                    :doc "Registers fold handlers"
                    :struct '(:modes list :priority int :triggers
                              (:delete fn :open-all fn :close-all-fn :toggle fn :open fn :open-rec fn :close fn))

                    ;; The body of the declaration:
                    ;; This gets put in a loop, its result `collect`ed into `evil-fold-list`
                    (append (list (* -1 (or (plist-get val :priority) 0)))
                            (list (ensure-list (plist-get val :modes)))
                            (plist-get val :triggers)
                            )
                    )

;; Then an instance of the spec is added:
;; `fold` is the name of the spec type.
;; `vimish` is the name of this spec instance.
;; Note the quasi-quote and commas, though you could just not function quote the functions
(spec-handling-add! fold
                    `(vimish
                     :modes (vimish-fold-mode)
                     :priority -50
                     :triggers (:delete     ,#'vimish-fold-delete
                                :open-all   ,#'vimish-fold-unfold-all
                                :close-all  ,#'vimish-fold-refold-all
                                :toggle     ,#'vimish-fold-toggle
                                :open       ,#'vimish-fold-unfold
                                :open-rec   nil
                                :close      ,#'vimish-fold-refold
                                )
                     )
                    )

;; Another fold instance.
;; This time called `outline`.
;; This has a higher priority than `vimish`, so will come first in `evil-fold-list`
(spec-handling-add! fold
                    `(outline
                     :modes (outline-mode outline-minor-mode markdown-mode)
                     :priority -25
                     :triggers (:open-all   ,#'outline-show-all
                                :close-all  ,(cmd! (with-no-warnings (outline-hide-sublevels 1)))
                                :toggle     ,#'outline-toggle-children
                                :open       ,(cmd! (with-no-warnings (outline-show-entry) (outline-show-children)))
                                :open-rec   ,#'outline-show-subtree
                                :close      ,#'outline-hide-subtree
                                )
                     )
                    )
                    
;; Up until now, nothing has changed in evil-fold-list
;; This call changes that:
(run-spec-handlers)

;; Some mode decides to fuck with evil-fold-list:
(setq evil-fold-list nil)

;; So you just call the handlers again:
(run-spec-handlers)


;; Check where you defined things:
(spec-handling-report)

```

