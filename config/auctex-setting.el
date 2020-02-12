;; Auto scan package directory.
;; (let ((default-directory  "~/.emacs.d/packages/"))
;;    (normal-top-level-add-subdirs-to-load-path))
;; Specify the corresponding load path
(add-to-list 'load-path "~/.emacs.d/packages/auctex")
(add-to-list 'load-path "~/.emacs.d/packages/cdlatex")
(load "auctex.el" nil t t)
(load "preview.el" nil t t)
(require 'cdlatex)
(setq Tex-auto-save t) ;; Enable TeX file auto save.
(setq outline-minor-mode-prefix [(control o)])  ;; Setting prefix key C-o of outline minor mode.
(setq inhibit-compacting-font-caches t) ;; Solve the problem of showing Unicode  characters.

(setq TeX-master nil) ;; Disable compile runtime request filename.
(setq TeX-parse-selt t) ;; Enable automatic resolution new file of TeX (usepackage, bibliograph, newtheorem etc.).
(setq LaTeX-indent-level 4) ;; Setting LaTeX indent level.

(setq TeX-PDF-mode t) 
(setq TeX-source-correlate-mode t) 
(setq TeX-source-correlate-method 'synctex) 
(setq TeX-view-program-list 
 '(("Sumatra PDF" ("\"E:/opt/SumatraPDF/SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search %b %n ") " %o")))) 

;; Setting autoload mode when open tex file.
(defun my-latex-hook ()
  "Hooks my cdlatex."
  (turn-on-cdlatex) ;; Load cdlatex.
  (outline-minor-mode) ;; Load outline mode.
  (turn-on-reftex)  ;; Load reftex.
  (auto-fill-mode)  ;; Enable autowrap.
  (electric-pair-mode t) ;; Enable autopair.
  (flyspell-mode)   ;; Load spell check (require aspell).
  (TeX-fold-mode t) ;; Load TeX fold mode.
  (outline-hide-body) ;; Enable auto hide when startup.
  (assq-delete-all (quote output-pdf) TeX-view-program-selection)    ;; Setting forward search.
  (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))
  (local-set-key [(control j)] 'my-newline) ;; Setting Ctrl+j for my-newline.
  )
;; With AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'my-latex-hook)
(provide 'auctex-setting)
