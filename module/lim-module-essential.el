(use-package emacs
  :ensure nil
  :config
  (setq mode-require-final-newline 'visit-save)
  (global-display-line-numbers-mode)
  (column-number-mode)
  (line-number-mode))

(use-package autorevert
  :ensure nil
  :config
  (setq auto-revert-verbose t)
  (global-auto-revert-mode))

(use-package emacs
  :ensure nil
  :demand t
  :config
  (setq tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct)
  (setq-default tab-width 2
                standard-indent 2
                indent-tabs-mode nil
                c-basic-offset 2))

(use-package emacs
  :config
  (global-set-key (kbd "C-<down-mouse-1>") 'ignore)
  (global-set-key [mode-line mouse-1] 'ignore)
  (global-set-key [mode-line mouse-2] 'ignore)
  (global-set-key [mode-line mouse-3] 'ignore))

(use-package mouse
  :ensure nil
  :hook (after-init . mouse-wheel-mode)
  :config
  (setq mouse-wheel-tilt-scroll t)
  (setq mouse-wheel-flip-direction t)
  (setq-default scroll-conservatively 1 ; affects `scroll-step'
                ;; scroll-preserve-screen-position 1 ; don't move when scroll
                scroll-margin 0
                next-screen-context-lines 0))

(use-package saveplace
  :ensure nil
  :config
  (setq save-place-forget-unreadable-files nil)
  :hook (after-init . save-place-mode))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-context-when-offscreen 'overlay))

(use-package electric
  :ensure nil
  :hook
  (prog-mode . electric-indent-local-mode))

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  ;; :init
  ;; (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  )

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package avy
  :config
  (setq avy-background t)
  (setq avy-all-windows t)
  (setq avy-timeout-seconds 0.3))

(use-package ace-window
  :defer t
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 1.0))

(use-package symbol-overlay
  :diminish
  :custom-face
  (symbol-overlay-default-face ((t :inherit (bold shadow) :underline (:color "gray"))))
  (symbol-overlay-face-1 ((t (:inherit nerd-icons-blue :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-2 ((t (:inherit nerd-icons-pink :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit nerd-icons-yellow :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit nerd-icons-purple :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit nerd-icons-red :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit nerd-icons-orange :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-7 ((t (:inherit nerd-icons-green :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit nerd-icons-cyan :background unspecified :foreground unspecified :inverse-video t))))
  :hook ((prog-mode . symbol-overlay-mode))
  :init (setq symbol-overlay-idle-time 0.3))

(use-package help
  :ensure nil
  :config
  (setq help-window-select t))

(use-package move-dup)

(use-package crux)

(use-package expreg)

(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all t))

(use-package hideshow
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

(use-package super-save
  :diminish nil
  :config
  (super-save-mode)
  (setq super-save-auto-save-when-idle t))

(use-package lim-helper
  :ensure nil
  :functions (lim-helper-truncate-lines-silently)
  :hook ((fundamental-mode text-mode prog-mode) . lim-helper-truncate-lines-silently)
  :config
  (advice-add #'execute-extended-command--describe-binding-msg :override #'lim-helper-ignore))

(use-package format-all
  :defer t
  :diminish nil
  :commands format-all-mode
  ;; :hook (prog-mode . format-all-mode)
  :bind ("C-c f" . #'format-all-region-or-buffer))

(use-package compile
  :ensure nil
  :functions (lim/compilation-colorize-buffer)
  :hook (compilation-filter . lim/compilation-colorize-buffer)
  :config
  (require 'lim-simple)
  (setq compilation-buffer-name-function #'lim-simple-compilation-buffer-name)
  (require 'ansi-color)
  (defun lim/compilation-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(use-package exec-path-from-shell
  ;; :autoload (exec-path-from-shell-initialize)
  :config (exec-path-from-shell-initialize))

(use-package lim-simple
  :ensure nil)

(use-package lim-scratch
  :ensure nil
  :bind
  ("C-c s" . lim-scratch-buffer))

(use-package elisp-mode
  :ensure nil
  :bind
  ( :map lisp-interaction-mode-map
    ("C-j" . nil)
    ("M-RET" . eval-print-last-sexp)))

(use-package persistent-scratch
  :diminish
  :bind (:map persistent-scratch-mode-map
         ([remap kill-buffer] . lim-scratch-confirm-kill-buffer)
         ([remap revert-buffer] . persistent-scratch-restore)
         ([remap revert-this-buffer] . persistent-scratch-restore))
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode))
  :init (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
              persistent-scratch-backup-directory
              (expand-file-name "persistent-scratch" user-emacs-directory)))

(use-package lim-pair :ensure nil)

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 100)
  (setq recentf-exclude `(,(recentf-expand-file-name "\\(straight\\|quelpa\\|elpa\\|eln-cache\\|var\\|.cache\\)/.*")
                          ,(recentf-expand-file-name "\\(auto-save-list\\|temp\\|transient\\|etc\\|backup\\|elfeed\\)/.*")
                          ,(recentf-expand-file-name "\\(recentf\\|bookmarks\\|savehist\\|places\\)")
                          ,tramp-file-name-regexp
                          "^/tmp" "\\.bak\\'" "\\.gpg\\'" "\\.gz\\'" "\\.tgz\\'" "\\.xz\\'" "\\.zip\\'" "^/ssh:" "\\.png\\'"
                          "\\.jpg\\'" "/\\.git/" "\\.gitignore\\'" "\\.log\\'" "COMMIT_EDITMSG" "\\.pyi\\'" "\\.pyc\\'"
                          ))

  ;; (run-at-time nil (* 5 60) #'recentf-save-list)
  (add-hook 'kill-emacs-hook #'recentf-save-list)
  ;; (add-hook 'kill-emacs-hook #'recentf-cleanup)
  )

(use-package bookmark
  :ensure nil
  :commands (bookmark-set bookmark-jump bookmark-bmenu-list)
  :hook (bookmark-bmenu-mode . hl-line-mode))

(use-package register
  :ensure nil
  :defer t
  :config
  (setq register-preview-delay 0.8
        register-preview-function #'register-preview-default)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'register-alist)))

(use-package tooltip
  :ensure nil
  :hook (after-init . tooltip-mode)
  :config
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.5
        x-gtk-use-system-tooltips t
        tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 10)
          (border-width . 0)
          (no-special-glyphs . t))))

(use-package server
  :ensure nil
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

(use-package goto-chg
  :bind
  (("C-(" . goto-last-change)
   ("C-)" . goto-last-change-reverse)))

(use-package view
  :ensure nil
  :functions (readonly-if-internal)
  :hook
  (emacs-lisp-mode . readonly-if-internal)
  (find-function-after . view-mode)
  (find-file . view-mode)
  :config
  (setcdr view-mode-map nil)
  (defun readonly-if-internal ()
    (let ((name (or (buffer-file-name) "")))
      (cond
       ((string-match "\\.el\\.gz\\'" name) (read-only-mode +1))
       ((string-match "\\*/elpa" name) (read-only-mode +1)))))
  )

(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode))

(use-package ediff
  :ensure nil
  :hook (ediff-quit . winner-undo))

(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

(use-package vundo
  :defer 1
  :bind
  (("C-c C-/" . vundo)
   :map vundo-mode-map
   ("C-/" . vundo-backward)
   ("b" . vundo-backward)
   ("f" . vundo-forward)
   ("g" . vundo-goto-last-saved)
   ("h" . vundo-backward)
   ("j" . vundo-next)
   ("k" . vundo-previous)
   ("l" . vundo-forward))
  :config
  (setq vundo-compact-display t)
  
  (with-eval-after-load 'pulsar
    (add-hook 'vundo-post-exit-hook #'pulsar-pulse-line-green)))

(use-package lim-search
  :ensure nil
  :commands (lim-search-mode lim-search-mode-toggle)
  :hook
  (lim-search-mode-quit . lim-search-dehighlight)
  ;; (lim-search-mode-quit . (lambda () (lazy-highlight-cleanup t)))
  ;; (lim-search-mode . isearch-exit)
  )

(use-package isearch
  :ensure nil
  :demand t
  :hook
  (isearch-mode-end . lim-search-dehighlight)
  :bind
  ( :map isearch-mode-map
    ([remap isearch-delete-char] . #'isearch-del-char)
    ([escape] . #'isearch-exit)
    ("C-c C-c" . #'lim-search-mode)
    )
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq lazy-highlight-cleanup nil))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        ;; frename buffer after killing uniquify
        uniquify-after-kill-buffer-p t
        ;; don't mock special buffers
        uniquify-ignore-buffers-re "^\\*"
        ))

(use-package dumb-jump
  :hook
  (xref-backend-functions . dumb-jump-xref-activate)
  :init
  ;; (setq dumb-jump-default-project user-emacs-directory)
  (setq dumb-jump-selector 'completing-read))

(use-package simple
  :ensure nil
  :config
  (setq-default fill-column 120))

(use-package popper
  :hook
  (after-init . popper-mode)
  (popper-mode-hook . popper-echo-mode)
  ;; :config
  ;; (setq popper-reference-buffers
  ;;       '("\\*Messages\\*"
  ;;         "^\\*eldoc"
  ;;         elisp-refs-mode
  ;;         flymake-diagnostics-buffer-mode
  ;;         (lambda (buf)
  ;;           (with-current-buffer buf
  ;;             (derived-mode-p
  ;;              '(compilation-mode
  ;;                comint-mode
  ;;                help-mode))))))
  )

(use-package shackle
  :config
  (setq popper-display-control nil)
  ;; (setq split-width-threshold 1)
  (setq shackle-default-rule '(:select t))
  (setq shackle-rules
      '((help-mode :select t :align right)
        (compilation-mode :select t :size 0.3 :align below)))
  (shackle-mode 1))

(use-package whitespace
  :ensure nil
  :diminish nil
  ;; :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-line-column nil)
  (setq whitespace-style
        '(face
          trailing
          lines-tail
          tabs
          tab-mark
          missing-newline-at-eof
          ))
  (global-whitespace-mode))

(use-package repeat
  :ensure nil
  ;; :hook (after-init . repeat-mode)
  :config
  (setq repeat-exit-key "<escape>"))

(use-package defrepeater
  :vc ( :url "https://github.com/alphapapa/defrepeater.el.git"
        :rev :newest))

(use-package anzu)

(use-package flymake
  :diminish
  ;; :bind ("C-c f" . flymake-show-buffer-diagnostics)
  :hook (prog-mode . flymake-mode)
  :init
  (setq flymake-no-changes-timeout nil)
  (setq flymake-fringe-indicator-position 'right-fringe))

(use-package sideline-flymake
  :diminish sideline-mode
  :custom-face
  (sideline-flymake-error ((t (:height 0.85 :italic t))))
  (sideline-flymake-warning ((t (:height 0.85 :italic t))))
  (sideline-flymake-success ((t (:height 0.85 :italic t))))
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'point)
  (setq sideline-backends-right '(sideline-flymake)))

(use-package newcomment
  :ensure nil
  :config
  (setq comment-empty-lines nil))

(use-package lim-soothing
  :ensure nil
  :hook (after-init . global-lim-soothing-mode)
  :bind
  ( :map lim-soothing-mode-map

    ;; comment
    ("M-;" . lim-soothing-comment)
    ("C-c M-;" . lim-soothing-comment-timestamp-keyword)

    ;; jump
    ("C-;" . avy-goto-char-timer)
    ("C-c C-j" . avy-resume)

    ;; move/duplicate
    ("M-p" . move-dup-move-lines-up)
    ("M-n" . move-dup-move-lines-down)
    ;; ("C-c C-d" . move-dup-duplicate-down)
    ("C-c C-d" . lim-simple-duplicate-line-or-region)
    ("C-c C-u" . move-dup-duplicate-up)

    ;; expreg
    ("C-M-w" . expreg-expand)
    ("C-M-s" . expreg-contract)

    ;; crux
    ("C-a" . crux-move-beginning-of-line)
    ("C-c C-k" . lim-simple-kill-whole-line-or-region)
    ("C-S-j" . crux-top-join-line)
    ("C-j" . crux-smart-open-line)
    ("C-o" . crux-smart-open-line-above)

    ;; multiple-cursor
    ("C-c m t" . mc/mark-all-like-this)
    ("C-c m m" . mc/mark-all-like-this-dwim)
    ("C-c m l" . mc/edit-lines)
    ("C-c m e" . mc/edit-ends-of-lines)
    ("C-c m a" . mc/edit-beginnings-of-lines)
    ("C-c m n" . mc/mark-next-like-this)
    ("C-c m p" . mc/mark-previous-like-this)
    ("C-c m s" . mc/mark-sgml-tag-pair)
    ("C-c m d" . mc/mark-all-like-this-in-defun)

    ;; ace window
    ([remap other-window] . ace-window)

    ;; fold
    ("C-." . hs-toggle-hiding)
    ))

(provide 'lim-module-essential)
