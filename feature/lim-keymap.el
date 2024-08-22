;;; lim-keymap.el --- Lim's Keymap -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Keymap for Lim

;;; Code:

(defun lim-keymap-get-keys (keymap)
  "Get keys of keymap."
  (let (keys)
    (map-keymap
     (lambda (key def)
       (cl-pushnew (key-description (vector key)) keys :test 'equal))
     (keymap-canonicalize keymap))
    keys))

(defun lim-keymap-get-bindings (keymap &optional prefix)
  "Get bindings of keymap."
  (let (bindings)
    (map-keymap
     (lambda (evt def)
       (let* ((key (vconcat prefix (list evt)))
              (key-desc (key-description key))
              (act-def (keymap-lookup keymap key-desc)))
         (cond (def
                (setf (alist-get key-desc bindings nil nil 'equal)
                      def))
               (t
                (when act-def
                  (setf (alist-get key-desc bindings nil nil 'equal)
                        act-def))))))
     (keymap-canonicalize keymap))
    bindings))

(provide 'lim-keymap)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
