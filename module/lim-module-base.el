(eval-and-compile
  (setq user-full-name "Aaron Liew")
  (setq user-mail-address "tabuyos@outlook.com"))

(use-package emacs
  :config
  (when (native-comp-available-p)
    (setq native-comp-async-report-warnings-errors 'silent)
    (setq native-compile-prune-cache t)))

(use-package emacs
  :config
  (setq create-lockfiles nil)
  (setq make-backup-files nil)
  (setq backup-inhibited nil))

(use-package emacs
  :ensure nil
  :config
  (setq x-alt-keysym 'meta)
  (setq x-stretch-cursor nil)
  (setq custom-file lim-customize-file)
  (setq custom-safe-themes t)
  ;; (setq initial-major-mode 'emacs-lisp-mode)
  (setq delete-by-moving-to-trash t))

(use-package emacs
  :ensure nil
  :config
  (setq global-mark-ring-max 200)
  (setq mark-ring-max 200)
  (setq kill-ring-max 200))

(provide 'lim-module-base)

;; (use-package with-proxy)
