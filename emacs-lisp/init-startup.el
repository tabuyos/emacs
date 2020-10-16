;;; package --- tabuyos-init --- init-startup.el
;;; Commentary:
;; This configuration for startup
;;; Code:

(eval-when-compile
  (require 'init-constant))

;; FullScreen-
(defvar fullscreen-p nil
  "Check if fullscreen is on or off")

(defun restore-default-screen-size ()
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND restore #xf120
      (w32-send-sys-command 61728)
    (set-frame-parameter nil 'fullscreen nil)))

(defun open-full-screen-size ()
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximaze #xf030
      (w32-send-sys-command 61488)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(defun toggle-fullscreen ()
  (interactive)
  (setq fullscreen-p (not fullscreen-p))
  (if fullscreen-p
      (open-full-screen-size)
    (restore-default-screen-size)))

(open-full-screen-size)
;; -FullScreen

(provide 'init-startup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-startup.el ends here
