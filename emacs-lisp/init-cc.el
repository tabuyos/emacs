;;; package --- tabuyos-init --- init-cc.el
;;; Commentary:
;; This configuration for cc mode(C/C++)
;;; Code:

(eval-when-compile
  (require 'init-constant))

;; CCLS-
(use-package ccls
  :defer t
  :if (not *sys/windows*)
  :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp)))
  :custom
  (ccls-executable (executable-find "ccls"))
  (ccls-sem-highlight-method 'font-lock)
  (ccls-enable-skipped-ranges nil)
  :config
  (lsp-register-clent
   (make-lsp-client
    :new-connection (lsp-tramp-connection (cons ccls-executable ccls-args))
    :major-modes '(c-mode c++-mode cuda-mode objc-mode)
    :server-id 'ccls-remote
    :multi-root nil
    :remote? t
    :notification-handlers
    (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
            ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
    :initialization-options (lambda () ccls-initialization-options)
    :library-folders-fn nil)))
;; -CCLS

;; CPPFontLock-
(use-package modern-cpp-font-lock
  :diminish t
  :init (modern-c++-font-lock-global-mode t))
;; -CPPFontLock

;; Smart-Compile-
(use-package smart-compile)
;; -Smart-Compile

;; Go-
(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save))
;; -Go

(provide 'init-cc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-cc.el ends here
