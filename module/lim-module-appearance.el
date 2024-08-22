(defvar emoji-family "Apple Color Emoji" "For emoji in MacOS.")

(defvar emoji-noto-family "NotoEmoji Nerd Font Mono")

(defvar symbol-family "Apple Symbols" "For symbol in MacOS.")

(defvar symbol-noto-family "Symbols Nerd Font Mono")

(defvar cjk-family "PingFang SC" "For CJK font(如汉字) in MacOS.")

(defvar cjk-noto-family "Noto Serif CJK SC")

(defvar kaiti-family "Kaiti SC")

(defvar code-family "JetBrains Mono")

(defvar default-family "Menlo" "For default font(just programing) in MacOS.")

(defvar default-noto-family "NotoMono Nerd Font")

(defvar fixed-family "NotoMono Nerd Font")

(defvar fixed-serif-family "NotoSerif Nerd Font Mono")

(defvar variable-family "NotoSans Nerd Font")

(defvar cjk-rescale-ratio 0.85)

(defun lim--rescale-font (family ratio)
  (if (/= ratio 0.0)
      (let ((elt (cons family ratio)))
        (unless (member elt face-font-rescale-alist)
          (push elt face-font-rescale-alist)))))

(defun lim--make-consistent-height (family)
  (let* ((font (face-attribute 'default :font))
         (size (font-get font :size))
         (name (format "%s-%s" family size))
         (height (aref (font-info name) 3))
         (modify (< (default-line-height) height)))
    (if modify
        (setq-default default-text-properties (list 'line-height height))
      )))

(defun lim-set-other-fontset-font ()
  (when window-system
    (setq use-default-font-for-symbols nil)

    (set-fontset-font t 'emoji (font-spec :family emoji-family))
    (set-fontset-font t 'symbol (font-spec :family symbol-family))

    (let* ((fontset (frame-parameter nil 'font))
           (ratio (or cjk-rescale-ratio 0.0))
           (family cjk-family)
           ;; (family cjk-noto-family)
           (fontspec (font-spec :family family)))

      (dolist (charset '(kana han hangul cjk-misc bopomofo))
        (set-fontset-font fontset charset fontspec))

      ;; Option 1
      (lim--rescale-font cjk-family ratio)

      ;; Option 2
      ;; (lim--make-consistent-height family)
      )))

;;; modus themes
(use-package modus-themes
  :demand t)

;; ef themes
(use-package ef-themes
  :demand t
  :config
  (setq ef-themes-variable-pitch-ui t)
  (setq ef-themes-mixed-fonts t)

  (defun lim/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'ef-day t))
      ('dark (load-theme 'ef-elea-dark t))))
  ;; (ef-themes-load-random 'dark)
  ;; (load-theme 'ef-day t)
  (add-hook 'ns-system-appearance-change-functions #'lim/apply-theme)
  )

(use-package pulsar
  :hook
  ((next-error . (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
   (minibuffer-setup . pulsar-pulse-line-red))
  :bind
  (("C-x l" . pulsar-pulse-line))
  :config
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)

  (pulsar-global-mode 1))

;; (use-package lin
;;   :hook
;;   (after-init . lin-global-mode)
;;   ((prog-mode text-mode) . lin-mode)
;;   :config
;;   (setq lin-face 'lin-green))

(use-package hl-line
  :ensure nil
  :functions (lim/set-cursor-fringe lim/cursor-fringe-local-buffer-run)
  :hook ((prog-mode text-mode) . lim/cursor-fringe-local-buffer-run)
  :config
  (defface lim/cursor-left-fringe-face
    '((t (:foreground "firebrick")))
    "Face for `lim/cursor-left-fringe-face'.")

  (defvar-local lim/cursor-left-fringe-overlay-position nil
    "Store cursor's position.

`overlay-arrow-bitmap' is a special SYMBOL defined in xdisp.c.")

  (defun lim/set-cursor-fringe ()
    "Set current cursor fringe."
    (if (not (and (eobp) (bolp)))
        (setq lim/cursor-left-fringe-overlay-position (copy-marker (line-beginning-position)))
      (setq lim/cursor-left-fringe-overlay-position  nil)))

  (defun lim/cursor-fringe-local-buffer-run ()
    "Registry fn to prog mode."
    (add-hook 'post-command-hook 'lim/set-cursor-fringe 'append 'local))

  ;; (define-fringe-bitmap 'lim/cursor-left-fringe-bitmap [128 192 96 48 24 48 96 192 128] 9 8 'center)
  (define-fringe-bitmap 'lim/cursor-left-fringe-bitmap [192 96 48 24 12 24 48 96 192] 9 8 'center)

  (set-fringe-bitmap-face 'lim/cursor-left-fringe-bitmap 'lim/cursor-left-fringe-face)

  (add-to-list 'overlay-arrow-variable-list 'lim/cursor-left-fringe-overlay-position)

  (put 'lim/cursor-left-fringe-overlay-position 'overlay-arrow-bitmap 'lim/cursor-left-fringe-bitmap))

(use-package spacious-padding
  ;; :if (display-graphic-p)
  :hook (after-init . spacious-padding-mode)
  :init
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 1
           :scroll-bar-width 8
           :left-fringe-width 20
           :right-fringe-width 20))
  (setq spacious-padding-subtle-mode-line
        '( :mode-line-active help-key-binding
           :mode-line-inactive window-divider))
  )

(use-package colorful-mode
  :diminish
  :hook (after-init . global-colorful-mode)
  :init
  (setq colorful-use-prefix t
        colorful-prefix-string "⬤")
  (setq colorful-allow-mouse-clicks nil)
  (dolist (mode '(html-mode help-mode helpful-mode))
    (add-to-list 'global-colorful-modes mode)))

;; (use-package rainbow-mode
;;   :diminish
;;   :commands rainbow-mode
;;   :init
;;   (setq rainbow-ansi-colors nil)
;;   (setq rainbow-x-colors nil)
;;   :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package cursory
  :demand t
  ;; :if (display-graphic-p)
  :config
  (setq cursory-presets
        '((box
           :blink-cursor-interval 0.8)
          (box-no-blink
           :blink-cursor-mode -1)
          (bar
           :cursor-type (bar . 2)
           :blink-cursor-interval 0.8)
          (bar-no-other-window
           :inherit bar
           :cursor-in-non-selected-windows nil)
          (bar-no-blink
           :cursor-type (bar . 2)
           :blink-cursor-mode -1)
          (underscore
           :cursor-type (hbar . 3)
           :blink-cursor-blinks 50)
          (underscore-thin-other-window
           :inherit underscore
           :cursor-in-non-selected-windows (hbar . 1))
          (underscore-thick
           :cursor-type (hbar . 8)
           :blink-cursor-interval 0.3
           :blink-cursor-blinks 50
           :cursor-in-non-selected-windows (hbar . 3))
          (underscore-thick-no-blink
           :blink-cursor-mode -1
           :cursor-type (hbar . 8)
           :cursor-in-non-selected-windows (hbar . 3))
          (t
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-mode 1
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))
  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))
  :hook
  (kill-emacs . cursory-store-latest-preset))

(use-package fontaine
  ;; :if (display-graphic-p)
  :init
  :hook
  ((after-init . fontaine-mode)
   (after-init . (lambda ()
                   (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))))
   (fontaine-set-preset . lim-set-other-fontset-font))
  :config
  (setq text-scale-mode-step 1.172)
  (setq x-underline-at-descent-line nil)
  (setq-default text-scale-remap-header-line t)
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  (setq fontaine-presets
        `((small
           :default-height 140)
          (regular) ;; use fallback values
          (medium
           :default-weight semilight
           :default-height 170
           :bold-weight extrabold)
          (large
           :inherit medium
           :default-height 180)
          (presentation
           :default-height 190)
          (t
           :default-family ,code-family
           :default-weight regular
           :default-height 160
           :fixed-pitch-family ,fixed-family
           :fixed-pitch-weight nil
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family ,fixed-serif-family
           :fixed-pitch-serif-weight nil
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family ,variable-family
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :line-spacing nil)))

  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

(use-package lim-modeline
  :ensure nil
  :config
  (require 'project)

  (setq mode-line-compact nil)
  (setq mode-line-right-align-edge 'right-margin)
  (setq-default mode-line-format
                '("%e"
                  lim-modeline-window-number
                  lim-modeline-buffer-status
                  lim-modeline-hydra
                  lim-modeline-kbd-macro
                  lim-modeline-narrow
                  lim-modeline-remote-status
                  lim-modeline-window-dedicated-status
                  lim-modeline-input-method
                  lim-modeline-major-mode
                  lim-modeline-minor-mode
                  lim-modeline-cursor-position
                  lim-modeline-selection
                  lim-modeline-encoding
                  lim-modeline-buffer-id-with-breadcrumb
                  lim-modeline-process
                  lim-modeline-vc
                  lim-modeline-eglot
                  ;; lim-modeline-notmuch-indicator
                  mode-line-format-right-align
                  lim-modeline-flymake
                  ;; project-mode-line-format
                  lim-modeline-misc-info
                  )))

;; (use-package keycast
;;   :after lim-modeline
;;   :commands (keycast-mode-line-mode keycast-header-line-mode keycast-tab-bar-mode keycast-log-mode)
;;   :init
;;   (setq keycast-mode-line-format "%2s(%K%1s%c%R)")
;;   (setq keycast-mode-line-insert-after 'lim-modeline-misc-info)
;;   (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
;;   (setq keycast-mode-line-remove-tail-elements nil)
;;   :config
;;   (dolist (input '(self-insert-command org-self-insert-command))
;;     (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

;;   (dolist (event '( mouse-event-p mouse-movement-p mwheel-scroll handle-select-window
;;                     mouse-set-point mouse-drag-region))
;;     (add-to-list 'keycast-substitute-alist `(,event nil))))

(use-package emacs
  :config
  (setq-default indicate-buffer-boundaries 'left))

(use-package breadcrumb
  :hook (after-init . breadcrumb-mode)
  :config
  (setq breadcrumb-project-max-length 0.5)
  (setq breadcrumb-project-crumb-separator "/")
  (setq breadcrumb-imenu-max-length 1.0)
  (setq breadcrumb-imenu-crumb-separator " > "))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":")
  (setq hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package minimap
  :defer t
  :config
  (setq minimap-window-location 'right)
  (setq minimap-update-delay 0)
  (setq minimap-width-fraction 0.09)
  (setq minimap-minimum-width 15))

(use-package page-break-lines
  :diminish
  ;; :hook (after-init . global-page-break-lines-mode)
  )

(provide 'lim-module-appearance)
