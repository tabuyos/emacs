;;; -*- coding: utf-8 -*-
;;; init-go.el
;;
;; @autor: tabuyos
;; @since: 1.0
;; @date: 2021-06-27
;;
;;; Commentary:
;;
;; This is initial configuration for golang.
;;
;;; Code:

;; GoMode-
(use-package go-mode
  :ensure t
  :commands go-mode
  :hook ((go-mode . lsp)
	 (before-save . gofmt-before-save))
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (setq gofmt-command "goimports")
  )
;; -GoMode

;; GoComplete-
(use-package go-complete
  :hook
  ((completion-at-point-functions . go-complete-at-point)))
(use-package go-autocomplete)
;; -GoComplete

(provide 'init-go)
;;; init-fonts.el ends here.
