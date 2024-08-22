;; (defvar lim--file-name-handler-alist file-name-handler-alist)
;; (defvar lim--vc-handled-backends vc-handled-backends)

;; (setq file-name-handler-alist nil
;;       vc-handled-backends nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(setq use-file-dialog nil
      use-dialog-box nil
      use-short-answers t)

(setq ring-bell-function 'ignore
      site-run-file nil)

(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

(setq frame-inhibit-implied-resize t)

(setq frame-title-format
      '((:eval (lim-simple-frame-title-format))))

(setq initial-scratch-message nil
      inhibit-startup-message t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message "Aaron Liew(aka Tabuyos)"
      inhibit-startup-buffer-menu t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))


;; Option 1: Customize frame size
(add-to-list 'default-frame-alist '(width . (text-pixels . 1500)))
(add-to-list 'default-frame-alist '(height . (text-pixels . 700)))
;; (add-to-list 'default-frame-alist '(undecorated-round . t))
;; (add-to-list 'default-frame-alist '(drag-internal-border . 1))
;; (add-to-list 'default-frame-alist '(internal-border-width . 5))

;; Option 2: Fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1
                  ;; file-name-handler-alist lim--file-name-handler-alist
                  ;; vc-handled-backends lim--vc-handled-backends
                  )))

;; (add-hook 'after-init-hook (lambda () (set-frame-name "home")))

(provide 'early-init)
