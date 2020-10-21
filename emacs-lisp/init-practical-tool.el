;;; package --- tabuyos-init --- init-practical-tool.el
;;; Commentary:
;; This is initial configuration for practical tool.
;;; Code:

;; Transparent-
(defvar alpha-list '((100 . 100) (95 . 65) (85 . 55) (75 . 45) (65 . 35) (55 . 25) (45 . 15) (35 . 10))
  "Alpha transparent list.")

(defun loop-alpha ()
  "Set Frame parameter."
  (interactive)
  (let ((cell (car alpha-list)))
    ((lambda (active inactive)
       (set-frame-parameter (selected-frame) 'alpha (list active inactive))
       (add-to-list 'default-frame-alist (cons 'alpha (list active inactive))))
     (car cell) (cdr cell))
    (setq alpha-list (cdr (append alpha-list (list cell))))))

(global-set-key (kbd "C-<f11>") #'loop-alpha)
;; -Transparent

;; TextProperties-
(defun set-text-read-only (start end &optional properties)
  "Set some text to read only with START and END and PROPERTIES."
  (if properties
      (set-text-properties start end properties)
    (set-text-properties start end '(read-only t rear-nonsticky t front-sticky t))))

(defun unset-text-read-only (start end)
  "Unset some text to read only with START and END."
  (let ((inhibit-read-only t))
    (set-text-properties start end ())))
;; -TextProperties

(provide 'init-practical-tool)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-practical-tool.el ends here
