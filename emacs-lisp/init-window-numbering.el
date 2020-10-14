;;; package --- tabuyos-init --- init-window-numbering.el
;;; Commentary:
;; This is initial configuration for window number.
;;; Code:

;; WindowNumbering-
(use-package window-numbering
  :load-path (lambda () (expand-file-name "site-elisp/window-numbering.el" user-emacs-directory))
  :config
  (window-numbering-mode 1))
;; -WindowNumbering

(provide 'init-window-numbering)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window-numbering.el ends here
