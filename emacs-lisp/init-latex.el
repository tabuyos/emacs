;;; package --- tabuyos-init
;;; Commentary:
;; This is configuration for the latex.
;;; Code:

(eval-when-compile
  (require 'init-constant)
  (require 'init-global-config)
  (require 'init-function))

;; AUCTeX-
(use-package tex
  :ensure auctex
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master t)
  (TeX-view-program-selection '((output-pdf "sumatra-pdf"))
                              TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("sumatra-pdf" ("SumatraPDF.exe -reuse-instance" (mode-io-correlate " -forward-search %b %n ") " %o")) ("pdf-tools" "TeX-pdf-tools-sync-view")))
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :hook
  (LaTeX-mode . latex-custom-hook)
  :config
  (when (version< emacs-version "26")
    (add-hook LaTeX-mode-hook #'display-line-numbers-mode)))

(defun latex-custom-hook ()
  "`LaTeX-mode' for customization hook."
  (turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (reftex-isearch-minor-mode)
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run XeLaTeX"))
  (setq TeX-command-default "XeLaTeX"))
;; -AUCTeX

(provide 'init-latex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-latex.el ends here
