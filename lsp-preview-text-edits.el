;; -*- lexical-binding: t; read-symbol-shorthands: (("my" . "lsp-preview-text-edits--")) -*-

(require 'lsp-mode)
(require 'dash)
(require 'cl-lib)

(defface lsp-preview-text-edit-remove-text
  '((t (:strike-through "RoyalBlue4")))
  ""
  :group 'lsp-diagnostics)


(defface lsp-preview-text-edit-add-text
  '((t (:background "Paleturquoise3")))
  ""
  :group 'lsp-diagnostics)


(defface lsp-preview-text-edit-replace-text
  '((t (:background "Paleturquoise3")))
  ""
  :group 'lsp-diagnostics)


(defcustom lsp-preview-text-edits-captured-text-edit-type
  'all
  "The types of text edit operations which can be previewed. Be
careful with changing this as some types of text edits are called
in contexts where previewing makes no sense and could randomly
break (e.g. text edits on key press for automatic re-format or
automated LSP server text edit requests)."
  :type
  '(choice
    (const :tag "Always capture" all)
    (repeat
     (choice
      (const :tag "No type (default)" nil)
      (const :tag "completion-cleanup" completion-cleanup)
      (const :tag "format" format)
      (const :tag "color-presentation" color-presentation)
      (const :tag "code-action" code-action)
      (const :tag "rename" rename)
      (const :tag "rename-file" rename-file)
      )
     ))
)

(defcustom lsp-preview-text-edits-previewing-allowed-predicates nil
  "Irregular hook run to determine if previewing is allowed in some context.
This is passed one parameter, which is non-nil exactly when the
preview would require a user confirmation/prompt")

(defvar my/lsp-previewing-text-edit-overlays nil)
(defvar my/lsp-previewing-text-edit-actions nil)
(defvar my/lsp-inside-previewing nil)

(defun my/symbol-name-safe(sym)
  (cond ((symbolp sym) (symbol-name sym))
        (t sym)))

(defun my/make-closure-for-text-edit-operation(fn &rest args)
  (let*
      ((save-buf (current-buffer))
       (save-point (point))
       (save-point-min (point-min))
       (save-point-max (point-max))
       )
    (push
     (lambda()
       (with-demoted-errors (format "command '%s' failed because %%s"
                                    (my/symbol-name-safe fn))
         (with-current-buffer save-buf
           (save-excursion
             (goto-char save-point)
             (save-restriction
               (widen)
               (narrow-to-region save-point-min save-point-max)
               (apply fn args)
               )
             )
           )
         )
       )
     my/lsp-previewing-text-edit-actions)
    )
  )

(lsp-defun my/lsp-preview-text-edit
  ((edit &as &TextEdit :range (&RangeToPoint :start :end) :new-text))

  ;;;; visualization the effect of:
  ;; (goto-char start)
  ;; (delete-region start end)
  ;; (insert new-text)

  (let*
      ((is-replace (> end start))
       (ov (make-overlay
            start
            end (current-buffer) t t))
       (new-text (s-replace "\r" "" (or new-text "")))
       (new-text (if is-replace (concat "→" new-text) new-text))
       (new-text
        (propertize
         new-text
         'face (if is-replace 'lsp-preview-text-edit-replace-text 'lsp-preview-text-edit-add-text)
         )
        )
       )
    (overlay-put ov 'face 'lsp-preview-text-edit-remove-text)
    (overlay-put ov 'after-string new-text)
    ov
    )
)

(defun my/lsp-preview-text-edits (edits)
  (->> edits
       ;; We sort text edits so as to apply edits that modify latter
       ;; parts of the document first. Furthermore, because the LSP
       ;; spec dictates that: "If multiple inserts have the same
       ;; position, the order in the array defines which edit to
       ;; apply first."  We reverse the initial list and sort stably
       ;; to make sure the order among edits with the same position
       ;; is preserved.
       (nreverse)
       (seq-sort #'lsp--text-edit-sort-predicate)
       (-map #'my/lsp-preview-text-edit)
       (-filter #'identity)
       )
)

(defvar lsp-preview-text-edits-inhibit nil)

(defmacro with-inhibiting-lsp-preview-text-edits (&rest body)
  "Within the scope of BODY, never produce LSP text edit previews"
  `(let ((lsp-preview-text-edits-inhibit t)) ,@body))

(defun my-around/with-inhibiting-lsp-preview-text-edits (fn &rest args)
  (with-inhibiting-lsp-preview-text-edits (apply fn args)))

(defun my-around/lsp--apply-text-edits (fn edits &optional operation)
  (if (and my/lsp-inside-previewing
           (or (eq lsp-preview-text-edits-captured-text-edit-type 'all)
               (memq operation lsp-preview-text-edits-captured-text-edit-type))
           (not lsp-preview-text-edits-inhibit)
           )
      ;; create some overlays, save them for later deletion
      ;; and append the real action to be taken later
      (progn
        (setq my/lsp-previewing-text-edit-overlays
              (append my/lsp-previewing-text-edit-overlays
                      (my/lsp-preview-text-edits (copy-sequence edits))))
        (my/make-closure-for-text-edit-operation
         fn edits operation)
        )
    ;; perform the action immediately
    (funcall fn edits operation)
    )
)

(cl-defun my-around/create-lsp-edit-text-previewer (&key preview-only)
  (lambda (fn &rest args)
    (cond
     (my/lsp-inside-previewing
      ;; when re-entering don't do anything special, just call the wrapped
      ;; function.  this would typically be a very unusual case - the user
      ;; initiated an LSP edit command during another LSP edit command. however
      ;; we need to handle this whenever we advise a function with this wrapper,
      ;; and then call that function explicitly with the wrapper elsewhere
      (apply fn args))

     ;; explicitly skip preview in some context
     ((or lsp-preview-text-edits-inhibit
          (not (run-hook-with-args-until-failure
                'lsp-preview-text-edits-previewing-allowed-predicates preview-only)))
      (apply fn args))

     (t (let*
            ((my/lsp-previewing-text-edit-overlays nil)
             (my/lsp-previewing-text-edit-actions nil)
             (result)
             )
          (unwind-protect
              (progn

                ;; in the scope where the above variables are set, we except this function
                ;; to create the preview and save the actions for later
                (let ((my/lsp-inside-previewing fn))
                  (setq result
                        (apply fn args)))

                (when (not preview-only)
                  (cond
                   ;; if there is anything to preview, ask the user if they want to keep these changes
                   (my/lsp-previewing-text-edit-overlays
                    (let ((apply-edits (y-or-n-p "Apply edits?")))
                      (when apply-edits
                        (-each my/lsp-previewing-text-edit-actions #'funcall))
                      )
                    )

                   ;; if there is nothing to preview just execute any pending actions
                   (my/lsp-previewing-text-edit-actions
                    (-each my/lsp-previewing-text-edit-actions #'funcall))
                   )
                  )
                )

            ;; always clear the overlays when we used y-or-n-p
            ;; to determine whether to apply
            (when (not preview-only)
              (-each my/lsp-previewing-text-edit-overlays #'delete-overlay))
            )

          ;; return a function which removes the overlay preview
          (when preview-only
            (-partial
             #'-each my/lsp-previewing-text-edit-overlays #'delete-overlay)
            )
          )
        )
      )
    )
)

(defalias 'my-around/perhaps-lsp-will-edit-text
  (my-around/create-lsp-edit-text-previewer :preview-only nil))

(defalias 'my-around/preview-lsp-edit-text
  (my-around/create-lsp-edit-text-previewer :preview-only t))

(defvar my/lsp-text-edit-active-preview nil)

(defun lsp-text-edit-switch-active-preview (&optional new-preview)
  "Remove the current active preview and set the new active preview to the given one"
  (interactive)
  (when my/lsp-text-edit-active-preview
    (with-demoted-errors "Failed to remove old LSP text edit preview %s"
      (funcall my/lsp-text-edit-active-preview)))
  (setq my/lsp-text-edit-active-preview new-preview))

(defalias 'lsp-text-edit-hide-active-preview 'lsp-text-edit-switch-active-preview)

(defvar lsp-preview-text-edits-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "")

(defmacro my/advised-symbols-list-custom-setter
    (enable-when where function &optional props)
  `(lambda(sym val)
    ;; remove advice from all previous values
    (--each (and (boundp sym) (default-value sym))
      (advice-remove
       it ,function))

    (set-default sym val)

    ;; add advice to all new values
    (when ,enable-when
      (--each val
        (advice-add
         it ,where ,function ,props))
      ))
  )

(defcustom lsp-preview-text-edits-perhaps-will-edit
  '(lsp--execute-code-action
    lsp--before-save
    lsp-format-buffer
    lsp-rename
    lsp-format-region
    )
  "When `lsp-preview-text-edits-mode' is enabled, these symbols are
automatically advised to handle any LSP text edits generated
within the body of these functions."

  :type
  '(repeat symbol)

  :set
  (lambda(sym val)
    ;; remove advice from all previous values
    (--each (and (boundp sym) (default-value sym))
      (advice-remove
       it #'my-around/perhaps-lsp-will-edit-text))
    
    (set-default sym val)

    ;; add advice to all new values
    (when (and (boundp 'lsp-preview-text-edits-mode) lsp-preview-text-edits-mode)
      (--each val
        (advice-add
         it :around #'my-around/perhaps-lsp-will-edit-text))
      )
    )
)


(defcustom lsp-never-preview-text-edits
  '(indent-region)
  "Within the body of these functions, never try to preview text
edits."

  :type
  '(repeat symbol)

  :set
  (lambda(sym val)
    ;; remove advice from all previous values
    (--each (and (boundp sym) (default-value sym))
      (advice-remove
       it #'my-around/with-inhibiting-lsp-preview-text-edits))

    (set-default sym val)

    ;; add advice to all new values
    (--each val
      (advice-add
       it :around #'my-around/with-inhibiting-lsp-preview-text-edits)))
  )

(defun my/advice-add-or-remove(add-or-remove symbol where function &optional props)
  (if add-or-remove
      (advice-add symbol where function props)
    (advice-remove symbol function)))

(define-minor-mode lsp-preview-text-edits-mode
  "When this global minor mode is enabled, LSP commands which
modify text within the current buffer offer a preview before
those modifications are applied, with the opportunity to reject
the changes. "
  :init-value nil
  :global t
  :keymap lsp-preview-text-edits-mode-map

  (--each lsp-preview-text-edits-perhaps-will-edit
     (my/advice-add-or-remove
      lsp-preview-text-edits-mode
      it :around #'my-around/perhaps-lsp-will-edit-text))
)

(defun lsp-preview-text-edits-mode-toggle()
  (interactive)
  (lsp-preview-text-edits-mode 'toggle))

(advice-add 'lsp--apply-text-edits :around #'my-around/lsp--apply-text-edits)

(defmacro create-lsp-with-preview-text-edits-command(funsymbol)
  (let*
      ((funsymbol-str (symbol-name funsymbol))
       (funsymbol-quoted (list 'quote funsymbol))
       (fun-commandp (commandp funsymbol))
       (with-preview-docstring
        (format "Behaves like '%s' except that if that command would \
produce text edits from LSP, shows a preview of those edits and \
gives you the chance to reject them."
                funsymbol-str)
        )
       )
    `(progn
       (defun ,(intern (concat funsymbol-str "-with-preview"))
           ,(if fun-commandp () `(&rest args))
         ,with-preview-docstring
         ,@(if fun-commandp
               `((interactive)
                 (my-around/perhaps-lsp-will-edit-text
                  (lambda()
                    (call-interactively ,funsymbol-quoted))))
             `((my-around/perhaps-lsp-will-edit-text
                (lambda()
                  (apply ,funsymbol-quoted args))))
             )
         )
       )
    )
  )

(create-lsp-with-preview-text-edits-command lsp-execute-code-action)
(create-lsp-with-preview-text-edits-command lsp-format-buffer)
(create-lsp-with-preview-text-edits-command lsp-format-region)
(create-lsp-with-preview-text-edits-command lsp-rename)

;;; helm compat
(with-eval-after-load 'helm-lsp
  (defvar my/helm-lsp-code-actions-preview-actions
    `(("Execute code action" .
       ,(lambda(candidate)
          (with-inhibiting-lsp-preview-text-edits
            (lsp-execute-code-action (plist-get candidate :data)))
         )
       )))

  (defun my/helm-lsp-code-action-preview-candidate(candidate)
    (interactive)
    (setq candidate (plist-get candidate :data))
    (->> candidate
         (my-around/preview-lsp-edit-text #'lsp-execute-code-action)
         lsp-text-edit-switch-active-preview))

  (defvar my/helm-lsp-code-actions-preview-persistent-action
    '(my/helm-lsp-code-action-preview-candidate . never-split))

  (defun my/helm-lsp-code-actions-preview-make-source(actions)
    (helm-build-sync-source
        "Code Actions (With Preview)"
      :candidates actions
      :candidate-transformer
      (lambda (candidates)
        (-map
         (-lambda ((candidate &as
                              &CodeAction :title))
           (list title :data candidate))
         candidates))
      :action my/helm-lsp-code-actions-preview-actions
      :persistent-action 'my/helm-lsp-code-action-preview-candidate
      :cleanup #'lsp-text-edit-switch-active-preview
      ))

  ;; this is copied from `helm-lsp-code-actions' except that we replace the
  ;; `:sources' argument to `helm'
  (defun helm-lsp-code-actions-with-preview()
    "Show lsp code actions using helm."
    (interactive)
    (let ((actions (lsp-code-actions-at-point)))
      (cond
       ((seq-empty-p actions) (signal 'lsp-no-code-actions nil))
       ((and (eq (seq-length actions) 1) lsp-auto-execute-action)
        (lsp-execute-code-action (lsp-seq-first actions)))
       (t (helm :sources
                (my/helm-lsp-code-actions-preview-make-source actions))))))

  ;; there really isn't a good place to hook into to provide a new action to the
  ;; helm-lsp-code-actions source, so we just replace it entirely
  (add-hook 'lsp-preview-text-edits-mode-hook
            (lambda()
              (if lsp-preview-text-edits-mode
                  (advice-add 'helm-lsp-code-actions :override #'helm-lsp-code-actions-with-preview)
                (advice-remove 'helm-lsp-code-actions #'helm-lsp-code-actions-with-preview))))

  (add-hook 'lsp-preview-text-edits-previewing-allowed-predicates
            (lambda(preview-only)
              (if preview-only
                  ;; always allowed previewing without prompt
                  t

                ;; when there might be a prompt, disallow previewing if helm is
                ;; active this is not technically wrong, since helm allows the
                ;; minibuffer read during helm modes, but is potentially
                ;; confusing as helm actions don't typically have confirmations.
                (not helm-alive-p))))
)

(provide 'lsp-preview-text-edits)
