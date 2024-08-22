;;; lim-search.el --- Lim's Search Mode -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Search for Lim

;;; Code:

(defvar lim-search-mode-quit-hook nil
  "Function(s) to call after terminating lim's search.")

(defun lim-search-dehighlight ()
  "Dehighlight search."
  (isearch-dehighlight)
  (lazy-highlight-cleanup t))

;;;###autoload
(defun lim-search-mode-quit ()
  "Disable `lim-search-mode'."
  (interactive)
  (lim-search-mode 0)
  (run-hooks 'lim-search-mode-quit-hook))

;;;###autoload
(defun lim-search-mode-toggle ()
  "Toggle `lim-search-mode'."
  (interactive)
  (lim-search-mode 'toggle))

(defvar-keymap lim-search-mode-map
  "p" #'isearch-repeat-backward
  "n" #'isearch-repeat-forward
  "<escape>" #'lim-search-mode-quit)

;;;###autoload
(define-minor-mode lim-search-mode
  "Lim's search mode."
  :init-value nil
  :global nil
  :lighter " Qs"
  :keymap lim-search-mode-map)

(provide 'lim-search)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
