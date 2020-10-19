;;; package --- tabuyos-init --- init-slime.el
;;; Commentary:
;; This configuration for slime
;;; Code:

;; Slime-
(use-package slime
  :ensure t
  :defer t
  :init
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  :custom
  (inferior-lisp-program "sbcl")
  (slime-net-coding-system 'utf-8-unix)
  (slime-complete-symbol*-fancy t)
  (slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (slime-contribs '(slime-fancy slime-asdf slime-quicklisp slime-cl-indent))
  :config
  (use-package slime-company
    :ensure t)
  (slime-setup '(slime-fancy slime-asdf slime-quicklisp slime-company)))
;; -Slime

;; ;; Paredit-
;; (use-package paredit
;;   :ensure t
;;   :bind
;;   (("C-j" . gen-new-line-in-below))
;;   :hook
;;   ((lisp-interaction-mode
;;     ;; emacs-lisp-mode
;;     ielm-mode
;;     lisp-mode
;;     eval-expression-minibuffer-setup) . paredit-mode))
;; ;; -Paredit

;; ;; EasyKill-
;; (use-package easy-kill
;;   :ensure t
;;   :config
;;   (global-set-key [remap kill-ring-save] 'easy-kill))
;; ;; -EasyKill

(provide 'init-slime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-slime.el ends here
