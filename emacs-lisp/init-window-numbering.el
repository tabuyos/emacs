;;; -*- coding: utf-8 -*-
;;; init-window-numbering.el
;;
;; @autor: tabuyos
;; @since: 1.0
;; @date: 2021-07-04
;;
;;; Commentary:
;;
;; This is initial configuration for window number.
;;
;;; Code:

;; WindowNumbering-
(use-package window-numbering
  :load-path (lambda () (expand-file-name "site-elisp/window-numbering.el" user-emacs-directory))
  :config
  (window-numbering-mode 1))
;; -WindowNumbering

(provide 'init-window-numbering)
;;; init-window-numbering.el ends here.
