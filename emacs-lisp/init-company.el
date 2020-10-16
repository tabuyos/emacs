;;; package --- tabuyos-init --- init-company.el
;;; Commentary:
;; This configuration for company
;;; Code:

(eval-when-compile
  (require 'init-constant))

;; Company-
(use-package company
  :diminish company-mode
  :hook
  ((prog-mode
    LaTeX-mode
    latex-mode
    ess-r-mode
    js2-mode) . company-mode)
  :bind
  (:map company-active-map
	([tab] . smarter-yas-expand-next-field-complete)
	("TAB" . smarter-yas-expand-next-field-complete))
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-begin-commands '(self-insert-command))
  (company-require-match 'never)
  (company-global-modes '(not shell-mode eaf-mode))
  (company-idle-delay 0.1)
  (company-show-numbers t)
  :config
  (unless clangd-p (delete 'company-clang company-backends))
  (global-company-mode 1)
  (defun smarter-yas-expand-next-field-complete ()
    "Try to `yas-expand' and `yas-next-field' at current cursor position.
If failed try to complete the common part with `company-complete-common'"
    (interactive)
    (if yas-minor-mode
	(let ((old-point (point))
	      (old-tick (buffer-chars-modified-tick)))
	  (yas-expand)
	  (when (and (eq old-point (point))
		     (eq old-tick (buffer-chars-modified-tick)))
	    (ignore-errors (yas-next-field))
	    (when (and (eq old-point (point))
		       (eq old-tick (buffer-chars-modified-tick)))
	      (company-complete-common))))
      (company-complete-common))))
;; -Company

(provide 'init-company)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
