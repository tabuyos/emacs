;;; package --- tabuyos-init
;;; Commentary:
;; This is configuration for the latex.
;;; Code:

(eval-when-compile
  (require 'init-constant)
  (require 'init-global-config)
  (require 'init-function))

;; AUCTeX-
(use-package auctex
  :defer t
  :ensure t)

(use-package cdlatex
  :defer t
  :ensure t)

(use-package tex
  :ensure auctex
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master t)
  (TeX-view-program-selection '((output-pdf "okular"))
                              TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("sumatra-pdf" ("SumatraPDF.exe -reuse-instance" (mode-io-correlate " -forward-search %b %n ") " %o")) ("pdf-tools" "TeX-pdf-tools-sync-view") ("okular" ("okular --unique %o#src:%n%a") "okular")))
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :hook
  (LaTeX-mode . latex-custom-hook)
  :config
  (when (version< emacs-version "26")
    (add-hook LaTeX-mode-hook #'display-line-numbers-mode)))

(defun latex-custom-hook ()
  "`LaTeX-mode' for customization hook."
  (turn-on-cdlatex)
  (turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (reftex-isearch-minor-mode)
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run XeLaTeX"))
  (setq TeX-command-default "XeLaTeX")
  (local-set-key [(control j)] #'gen-new-line-in-below))

(defun tabuyos/get-pdf-reader ()
  "Tabuyos for get some pdf reader."
  (interactive)
  (if (not *sys/linux*)
      "Sumatra-pdf"
    "Okular"))
;; -AUCTeX

(provide 'init-latex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-latex.el ends here
