(use-package tabby
  :vc ( :url "https://github.com/alan-w-255/tabby.el.git")
  :bind
  (("C-'" . tabby-complete)
   :map tabby-completion-map
   ("<tab>" . tabby-accept-completion)
   ("C-<return>" . tabby-accept-completion-by-word)
   ("C-l" . tabby-accept-completion-by-line))
  :commands (tabby-accept-completion)
  :defines (taby-mode-map tabby--ongoing-request-id)
  ;; :hook
  ;; (go-ts-mode . tabby-mode)
  ;; (tuareg-mode . tabby-mode)
  ;; (haskell-mode . tabby-mode)
  :config
  (setq tabby-idle-delay 1.5))

(use-package gptel
  :ensure t
  :defer 1
  :bind
  ( :map gptel-mode-map
    ("<RET>" . gptel-send))
  :config
  (setq gptel-default-mode 'org-mode)
  :init
  (setq-default gptel-model "Qwen2-1.5B-Instruct")
  (setq-default gptel-backend
                (gptel-make-openai "tabby"
                  :stream t
                  :protocol "http"
                  :host "localhost:18080"
                  :key "auth_df7bf9e69c29473f8e2ba10eea95a048"
                  :models '("Qwen2-1.5B-Instruct"))
                )
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(provide 'lim-module-ai)
