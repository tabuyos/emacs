;;; package --- tabuyos-init
;;; Commentary:
;; This initializes dart
;;; Code:

;; LSP Mode-
(use-package lsp-mode :ensure t)
(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp))
;; -LSP Mode

;; Optional packages-
;; project management
(use-package projectile :ensure t)
(use-package yasnippet
  :ensure t
  ;; snipets
  :config (yas-global-mode))
;; UI for LSP
(use-package lsp-ui :ensure t)
;; Auto-complete
(use-package company :ensure t)
;; -Optional packages

;; Optional Flutter packages-
;; run app from desktop without emulator
(use-package hover :ensure t)
;; -Optional Flutter packages


(provide 'init-dart)
;;; init-constant.el ends here
