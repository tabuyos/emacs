;;; -*- coding: utf-8 -*-
;;; init-frame.el
;;
;; @autor: tabuyos
;; @since: 1.0
;; @date: 2021-06-27
;;
;;; Commentary:
;;
;; This is initial configuration for frame.
;;
;;; Code:

;; Transparent-
(defvar te/alpha-list '((100 . 100) (95 . 65) (85 . 55) (75 . 45) (65 . 35) (55 . 25) (45 . 15) (35 . 10))
  "Alpha transparent list.")

(defun te/loop-alpha ()
  "Set Frame parameter."
  (interactive)
  (let ((cell (car te/alpha-list)))
    ((lambda (active inactive)
       (set-frame-parameter (selected-frame) 'alpha (list active inactive))
       (add-to-list 'default-frame-alist (cons 'alpha (list active inactive))))
     (car cell) (cdr cell))
    (setq te/alpha-list (cdr (append te/alpha-list (list cell))))))

(global-set-key (kbd "C-<f11>") #'te/loop-alpha)
;; -Transparent

;; TextProperties-
(defun te/set-text-read-only (start end &optional properties)
  "Set some text to read only with START and END and PROPERTIES."
  (if properties
      (set-text-properties start end properties)
    (set-text-properties start end '(read-only t rear-nonsticky t front-sticky t))))

(defun te/unset-text-read-only (start end)
  "Unset some text to read only with START and END."
  (let ((inhibit-read-only t))
    (set-text-properties start end ())))
;; -TextProperties

(provide 'init-frame)
;;; init-frame.el ends here.
