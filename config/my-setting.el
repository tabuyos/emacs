(defun shift-text (distance)
  (if (use-region-p)
    (let ((mark (mark)))
      (save-excursion
        (indent-rigidly (region-beginning)
          (region-end)
          distance)
        (push-mark mark t t)
        (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
      (line-end-position)
      distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))

(provide 'my-setting)