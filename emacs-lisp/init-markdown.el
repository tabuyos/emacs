;;; package --- tabuyos-init --- init-markdown.el
;;; Commentary:
;; This configuration for tabuyos
;;; Code:

;; MarkdownPreviewMode-
(use-package markdown-preview-mode
  :ensure t
  :init
  (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . markdown-preview-mode))
;; -MarkdownPreviewMode


(defun tabuyos/markdown-to-html ()
  "Preview markdown in browsers."
  (interactive)
  (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) "5000")
  (browse-url (format "http://localhost:5000/%s" (get-file-name (buffer-file-name)))))

(provide 'init-markdown)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markdown.el ends here
