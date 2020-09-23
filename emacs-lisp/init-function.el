;;; package --- tabuyos-init
;;; Commentary:
;; This is initializing functions file
;;; Code:

(eval-when-compile
  (require 'init-global-config))

;; ResizeWidtHeight-
;; Resizes the window width based on the input
(defun resize-window-width (w)
  "Resize the window width based on W."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current widow width in [1~9]x10%: ")
                       (error "You need more than 1 window to Execute this function!"))))
  (message "%s" w)
  (window-resize nil (- (truncate (* (/ w 10.0) (frame-width))) (window-total-width)) t))

;; Resizes the window height based on the input
(defun resize-window-height (h)
  "Resized the window height based on H."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window height in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!"))))
  (message "%s" h)
  (window-resize nil (- (truncate (* (/ h 10.0) (frame-height))) (window-total-height)) nil))

;; Setup shorcuts for window resize width and height
(global-set-key (kbd "C-z w") #'resize-window-width)
(global-set-key (kbd "C-z h") #'resize-window-height)

(provide 'init-function)
;;; init-function.el ends here
