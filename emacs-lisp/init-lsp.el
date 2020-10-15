;;; package --- tabuyos-init --- init-lsp.el
;;; Commentary:
;; This configuration for lsp
;;; Code:

;; LSPMode-
(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil)
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  (lsp-eldoc-hook nil)
  :bind
  (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook
  ((java-mode
    python-mode
    go-modejs-mode
    js2-mode
    typescript-mode
    web-mode
    c-mode
    c++-mode
    objc-mode) .lsp))
;; -LSPMode

;; LSPUI-
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
  (:map lsp-ui-mode-map
	([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	([remap xref-find-references] . lsp-ui-peek-find-references)
	("C-c u" . lsp-ui-imenu)
	("M-i" . lsp-ui-doc-focus-frame))
  (:map lsp-mode-map
	("M-n" . forward-paragraph)
	("M-p" . backward-paragraph))
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  (if (display-graphic-p)
      (setq lsp-ui-doc-use-webkit t))
  (define-advice lsp-ui-imenu (:after nil hide-lsp-ui-imenu-mode-line)
    (setq mode-line-format nil)))
;; -LSPUI

;; DAPMode-
(use-package dap-mode
  :bind
  (:map dap-mode-map
	(("<f12>" . dap-debug)
	 ("<f8>" . dap-continue)
	 ("<f9>" . dap-next)
	 ("M-<f11>" . dap-step-in)
	 ("C-M-<f11>" . dap-step-out)
	 ("<f7>" . dap-breakpoint-toggle))))
;; -DAPMode

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
