;;; -*- coding: utf-8 -*-
;;; init-startup.el
;;
;; @autor: tabuyos
;; @since: 1.0
;; @date: 2021-07-04
;;
;;; Commentary:
;;
;; This is configuration file for startup.
;;
;;; Code:

(eval-when-compile
  (require 'init-constant))

;; FullScreen-
(defvar te/fullscreen-p nil
  "Check if fullscreen is on or off.")

(defun te/restore-default-screen-size ()
  "Restore default screen size."
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND restore #xf120
      (w32-send-sys-command 61728)
    (set-frame-parameter nil 'fullscreen nil)))

(defun te/open-full-screen-size ()
  "Open fullscreen size."
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximaze #xf030
      (w32-send-sys-command 61488)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(defun te/toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (setq te/fullscreen-p (not te/fullscreen-p))
  (if te/fullscreen-p
      (te/open-full-screen-size)
    (te/restore-default-screen-size)))

;; (te/open-full-screen-size)
;; -FullScreen

(provide 'init-startup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-startup.el ends here
