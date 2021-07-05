;;; -*- coding: utf-8 -*-
;;; init-treemacs.el
;;
;; @autor: tabuyos
;; @since: 1.0
;; @date: 2021-07-04
;;
;;; Commentary:
;;
;; This is initial configuration for treemacs.
;;
;;; Code:

;; TreemacsPackage-
(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :custom
  (treemacs-collapse-dirs 3)
  (treemacs-deferred-git-apply-delay 0.5)
  (treemacs-display-in-side-window t)
  (treemacs-file-event-delay 5000)
  (treemacs-file-follow-delay 0.2)
  (treemacs-follow-after-init t)
  (treemacs-follow-recenter-distance 0.1)
  (treemacs-git-command-pipe "")
  (treemacs-gogo-tag-strategy 'refetch-index)
  (treemacs-indextation 2)
  (treemacs-indextation-string " ")
  (treemacs-is-never-other-window nil)
  (treemacs-max-git-entries 5000)
  (treemacs-no-png-images nil)
  (treemacs-no-delete-other-windows t)
  (treemacs-project-follow-cleanup nil)
  (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-recenter-after-file-follow nil)
  (treemace-recenter-after-tag-follow nil)
  (treemacs-show-cursor nil)
  (treemacs-show-hidden-files t)
  (treemacs-silent-filewatch nil)
  (treemacs-silent-refresh nil)
  (treemacs-sorting 'alphabetic-desc)
  (treemacs-space-between-root-nodes t)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.5)
  (treemacs-width 35)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (setq treemacs-sorting 'alphabetic-asc)
  :bind
  (("M-0"	. treemacs-select-window)
   ("C-x t 1"	. treemacs-delete-other-windows)
   ("C-x t t"	. treemacs)
   ("C-x t B"	. treemacs-bookmark)
   ("C-x t C-t"	. treemacs-find-file)
   ("C-x t M-t"	. treemacs-find-tag))
  (:map treemacs-mode-map ("C-p" . treemacs-previous-line)))
;; -TreemacsPackage

;; TreeMagit-
(use-package treemacs-magit
  :defer t
  :after (treemacs magit))
;; -TreeMagit

;; TreeProject-
(use-package treemacs-projectile
  :defer t
  :after (treemacs projectile))
;; -TreeProject

(provide 'init-treemacs)
;;; init-treemacs.el ends here
