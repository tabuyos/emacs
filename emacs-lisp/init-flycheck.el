;;; package --- tabuyos-init --- init-flycheck.el
;;; Commentary:
;; This configuration for flycheck
;;; Code:

;; FlyCheck-
(use-package flycheck
  :defer t
  :hook ((prog-mode markdown-mode) . flycheck-mode)
  :custom
  (flycheck-global-modes
   '(not text-mode outline-mode fundamental-mode org-mode
	 diff-mode shell-mode eshell-mode term-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe)
  :init
  (use-package flycheck-grammarly :defer t)
  (if (display-graphic-p)
      (use-package flycheck-posframe
	:custom-face (flycheck-posframe-border-face ((t (:inherit default))))
	:hook (flycheck-mode . flycheck-posframe-mode)
	:custom
	(flycheck-posframe-border-width 1)
	(flycheck-posframe-inhibit-functions
	 '((lambda (&rest _) (dound-and-true-p company-backend)))))
    (use-package flycheck-pos-tip
      :defines flycheck-pos-tip-timeout
      :hook (flycheck-mode . flycheck-pos-tip-mode)
      :custom (flycheck-pos-tip-timeout 30)))
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))
;; -FlyCheck

(provide 'init-flycheck)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
