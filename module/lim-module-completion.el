(defun lim-orderless--consult-suffix ()
  "Regexp which matches the end of string with Consult tofu support."
  (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
      (format "[%c-%c]*$"
              consult--tofu-char
              (+ consult--tofu-char consult--tofu-range -1))
    "$"))

(defun lim-orderless-literal (word _index _total)
  "Read WORD= as a literal string."
  (when (string-suffix-p "=" word)
    ;; The `orderless-literal' is how this should be treated by
    ;; orderless.  The `substring' form omits the `=' from the
    ;; pattern.
    `(orderless-literal . ,(substring word 0 -1))))

(defun lim-orderless-consult-dispatch (word _index _total)
  (cond
   ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
   ((string-suffix-p "$" word)
    `(orderless-regexp . ,(concat (substring word 0 -1) (lim-orderless--consult-suffix))))
   ;; File extensions
   ((and (or minibuffer-completing-file-name
             (derived-mode-p 'eshell-mode))
         (string-match-p "\\`\\.." word))
    `(orderless-regexp . ,(concat "\\." (substring word 1) (lim-orderless--consult-suffix))))))

(defun lim-orderless-beg-or-end (word _index _total)
  "Expand WORD~ to \\(^WORD\\|WORD$\\)."
  (when-let (((string-suffix-p "~" word))
             (word (substring word 0 -1)))
    `(orderless-regexp . ,(format "\\(^%s\\|%s$\\)" word word))))

(defun lim-orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-resize t))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind
  ( :map vertico-map
    ("RET" . vertico-directory-enter)
    ("DEL" . vertico-directory-delete-char)
    ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package emacs
  :init
  (defun lim/crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'lim/crm-indicator))

(use-package marginalia
  :defer 1
  :bind
  ( :map minibuffer-local-map
    ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0))

(use-package orderless
  :demand t
  :after minibuffer
  :bind ( :map minibuffer-local-completion-map
          ("SPC" . nil)
          ("?" . nil))
  :config
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq orderless-matching-styles '(orderless-prefixes orderless-literal orderless-regexp))

  (setq orderless-style-dispatchers
        '(lim-orderless-literal
          lim-orderless-beg-or-end
          lim-orderless-consult-dispatch
          orderless-affix-dispatch))

  (setq orderless-component-separator #'orderless-escapable-split-on-space)

  ;; (setq completion-styles '(substring initials flex orderless basic))
  (setq completion-styles '(substring partial-completion initials flex orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles . (partial-completion orderless basic)))
          (bookmark (styles . (orderless basic)))
          (library (styles . (orderless basic)))
          (lsp-capf (styles . (orderless basic)))
          (embark-keybinding (styles . (substring orderless basic)))
          (imenu (styles . (substring orderless basic)))
          (consult-location (styles . (substring orderless basic)))
          (kill-ring (styles . (substring orderless basic)))
          (eglot (styles . (substring orderless basic)))
          (eglot-capf (styles . (substring orderless basic)))
          (command (styles +orderless-with-initialism))
          (variable (styles +orderless-with-initialism))
          (symbol (styles +orderless-with-initialism)))))

(use-package emacs
  :config
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq-default case-fold-search t)
  (setq read-file-name-completion-ignore-case t))

(use-package mb-depth
  :ensure nil
  :hook
  ((after-init . minibuffer-depth-indicate-mode)
   (minibuffer-setup . cursor-intangible-mode))
  :config
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (setq read-minibuffer-restore-windows nil)
  (setq enable-recursive-minibuffers t))

(use-package minibuf-eldef
  :ensure nil
  :hook (after-init . minibuffer-electric-default-mode)
  :config
  (setq minibuffer-default-prompt-format " [%s]"))

(use-package rfn-eshadow
  :ensure nil
  :config
  (setq resize-mini-windows t)
  (setq read-answer-short t)
  (setq echo-keystrokes 0.25)
  (file-name-shadow-mode 1))

(use-package minibuffer
  :ensure nil
  :demand t
  :config
  (setq completions-format 'one-column)
  (setq completion-show-help nil)
  (setq completion-auto-help 'always)
  (setq completion-auto-select nil)
  (setq completions-detailed t)
  (setq completion-show-inline-help nil)
  (setq completions-max-height 6)
  (setq completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
  (setq completions-highlight-face 'completions-highlight)
  (setq minibuffer-completion-auto-choose t)
  (setq minibuffer-visible-completions t)
  (setq completions-sort 'historical))

(use-package corfu
  :hook ((after-init . global-corfu-mode))
  :bind ( :map corfu-map
          ("<tab>" . corfu-complete)
          :map corfu-popupinfo-map
          ("C-M-j" . corfu-popupinfo-scroll-up)
          ("C-M-k" . corfu-popupinfo-scroll-down))
  :config
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  ;; (setq corfu-auto-delay 0.1)
  (setq corfu-separator ?\s)
  ;; (setq corfu-preselect 'prompt)
  (setq corfu-scroll-margin 5)
  ;; (setq corfu-on-exact-match nil)
  (setq corfu-popupinfo-delay '(0.1 . 0.2))
  (setq corfu-popupinfo-max-width 140)
  (setq corfu-popupinfo-max-height 30)

  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (corfu-popupinfo-mode 1)

  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package cape
  :bind (("C-c p p" . completion-at-point)
         ("C-c p t" . complete-tag)
         ("C-c p d" . cape-dabbrev)
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-hook 'completion-at-point-functions #'cape-history)
  ;;(add-hook 'completion-at-point-functions #'cape-keyword)
  ;;(add-hook 'completion-at-point-functions #'cape-tex)
  ;;(add-hook 'completion-at-point-functions #'cape-sgml)
  ;;(add-hook 'completion-at-point-functions #'cape-rfc1345)
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  ;;(add-hook 'completion-at-point-functions #'cape-dict)
  ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-hook 'completion-at-point-functions #'cape-line)

  :config
  (setq dabbrev-check-other-buffers nil
        dabbrev-check-all-buffers nil
        cape-dabbrev-min-length 3)
  (cape-wrap-prefix-length #'cape-dabbrev 3)
)

(use-package consult
  :bind (;("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ;; ("C-c i" . consult-info)
         ;; ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("M-s b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ;; ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         :map consult-narrow-map
         ("?" . consult-narrow-help)
         :map isearch-mode-map
         ;; ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (advice-add #'register-preview :override #'consult-register-window)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  (setq consult-preview-key 'any)

  (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))

  (require 'consult-imenu)
  ;; (with-eval-after-load 'pulsar
  ;;     (setq consult-after-jump-hook nil)
  ;;     (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
  ;;       (add-hook 'consult-after-jump-hook fn)))
  )

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-delete-duplicates t)
  (setq history-length 600)
  (setq savehist-save-minibuffer-history t)
  (setq savehist-autosave-interval 300)
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'global-mark-ring)
  (add-to-list 'savehist-additional-variables 'extended-command-history))

(use-package embark
  :bind
  (("C-c C-a" . embark-act)
   ("C-c C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("C-c C-c" . embark-collect)
   ("C-c C-e" . embark-export))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :after grep
  :bind
  ( :map grep-mode-map
    ("e" . wgrep-change-to-wgrep-mode)
    ("C-x C-q" . wgrep-change-to-wgrep-mode)
    ("C-c C-c" . wgrep-finish-edit))
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(use-package grep
  :config
  (setq grep-highlight-matches t)
  ;; (setq grep-find-ignored-directories
  ;;       (append (list ".git" ".cache" "vendor" "node_modules" "target")
  ;;               grep-find-ignored-directories))
  ;; (setq grep-find-ignored-files
  ;;       (append (list "*.blob" "*.gz" "TAGS" "projectile.cache" "GPATH" "GRTAGS" "GTAGS" "TAGS" ".project" )
  ;;               grep-find-ignored-files))
  )

(use-package dabbrev
  :ensure nil
  :commands (dabbrev-expand dabbrev-completion)
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  (setq dabbrev-ignored-buffer-modes
        '(archive-mode image-mode docview-mode pdf-view-mode)))

(use-package tempel
  :bind
  (("M-+" . tempel-complete)
   ("M-*" . tempel-insert)
   :map tempel-map
   ("<tab>" . tempel-next)
   ("<backtab>" . tempel-previous))
  :init
  (setq tempel-path (expand-file-name "templates/*.eld" user-emacs-directory))
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions (cons #'tempel-expand completion-at-point-functions)))
  :hook
  (conf-mode . tempel-setup-capf)
  (prog-mode . tempel-setup-capf)
  (text-mode . tempel-setup-capf)
  (eglot-managed-mode . tempel-setup-capf)
  :functions (tempel-include)
  :config
  (global-tempel-abbrev-mode)
  (defun tempel-include (elt)
    (when (eq (car-safe elt) 'i)
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))
  (add-to-list 'tempel-user-elements #'tempel-include))

(use-package yasnippet
  ;; :config
  ;; (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after (yasnippet))

;; (use-package yasnippet-capf
;;   :after cape
;;   :config
;;   (add-hook 'completion-at-point-functions #'yasnippet-capf))

(use-package tempel-collection)

(provide 'lim-module-completion)
