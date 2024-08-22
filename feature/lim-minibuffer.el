;;; lim-minibuffer.el --- Enhanced Minibuffer -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Enhanced Minibuffer

;;; Code:

(require 'lim-hydra)

;;;###autoload
(defun lim-minibuffer-mode-enable ()
  "Enable `lim-minibuffer-mode'"
  (interactive)
  (lim-minibuffer-mode 1))

;;;###autoload
(defun lim-minibuffer-mode-disable ()
  "Disable `lim-minibuffer-mode'"
  (interactive)
  (lim-minibuffer-mode -1))

;;;###autoload
(defun lim-minibuffer-keyboard-quit ()
  "Keyboard Quit."
  (interactive)
  (minibuffer-keyboard-quit)
  (lim-minibuffer-mode-disable))

;;;###autoload
(defun lim-minibuffer-enter ()
  (interactive)
  (lim-hydra-enter)
  (lim-minibuffer-mode-disable))

;;;###autoload
(defun lim-minibuffer-insert-comma ()
  (interactive)
  (lim-simple-insert-comma)
  (lim-minibuffer-mode-disable))

(defvar lim-minibuffer-mode-map
  (let ((keymap (make-sparse-keymap)))
    (keymap-set keymap "," 'lim-minibuffer-insert-comma)
    (keymap-set keymap "n" 'lim-hydra-next-line)
    (keymap-set keymap "p" 'lim-hydra-prev-line)
    (keymap-set keymap "g" 'lim-minibuffer-keyboard-quit)
    (keymap-set keymap "SPC" 'lim-minibuffer-mode-disable)
    keymap))

;;;###autoload
(define-minor-mode lim-minibuffer-mode
  "Enhanced Minibuffer Navigate."
  :init-value nil
  :global nil
  :lighter nil
  :keymap lim-minibuffer-mode-map)

(provide 'lim-minibuffer)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
