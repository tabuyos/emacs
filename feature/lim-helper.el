;;; lim-helper.el --- The Helper for Lim -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Lim's Helper Library.

;;; Code:

(require 's)
(require 'f)

(defvar lim-helper-line-regexp-alist
  '((empty . "[\s\t]*$")
    (indent . "^[\s\t]+")
    (non-empty . "^.+$")
    (list . "^\\([\s\t#*+]+\\|[0-9]+[^\s]?[).]+\\)")
    (heading . "^[=-]+"))
  "Alist of regexp types used by `lim-helper-line-regexp-p'.")

(defun lim-helper-seconds-to-minutes (seconds)
  "Convert a number representing SECONDS to MM:SS notation."
  (let ((minutes (/ seconds 60))
        (seconds (% seconds 60)))
    (format "%.2d:%.2d" minutes seconds)))

(defun lim-helper-seconds-to-hours (seconds)
  "Convert a number representing SECONDS to HH:MM:SS notation."
  (let* ((hours (/ seconds 3600))
         (minutes (/ (% seconds 3600) 60))
         (seconds (% seconds 60)))
    (format "%.2d:%.2d:%.2d" hours minutes seconds)))

;;;###autoload
(defun lim-helper-seconds-to-minutes-or-hours (seconds)
  "Convert SECONDS to either minutes or hours, depending on the value."
  (if (> seconds 3599)
      (lim-helper-seconds-to-hours seconds)
    (lim-helper-seconds-to-minutes seconds)))

;;;###autoload
(defun lim-helper-empty-buffer-p ()
  "Test whether the buffer is empty."
  (or (= (point-min) (point-max))
      (save-excursion
        (goto-char (point-min))
        (while (and (looking-at "^\\([a-zA-Z]+: ?\\)?$")
                    (zerop (forward-line 1))))
        (eobp))))

;;;###autoload
(defun lim-helper-minor-modes-active ()
  "Return list of active minor modes for the current buffer."
  (let ((active-modes))
    (mapc (lambda (m)
            (when (and (boundp m) (symbol-value m))
              (push m active-modes)))
          minor-mode-list)
    active-modes))

;;;###autoload
(defun lim-helper-truncate-lines-silently ()
  "Toggle line truncation without printing messages."
  (let ((inhibit-message t))
    (toggle-truncate-lines t)))

;;;###autoload
(defun lim-helper-ignore (&rest _)
  "Use this as override advice to make a function do nothing."
  nil)

;;;###autoload
(defun lim-helper-clear-minibuffer-message (&rest _)
  "Print an empty message to clear the echo area.
Use this as advice :after a noisy function."
  (message ""))

;;;###autoload
(defun lim-helper-window-small-p ()
  "Return non-nil if window is small.
Check if the `window-width' or `window-height' is less than
`split-width-threshold' and `split-height-threshold',
respectively."
  (or (and (numberp split-width-threshold)
           (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
           (> (window-total-height) split-height-threshold))))

;;;###autoload
(defun lim-helper-window-narrow-p ()
  "Return non-nil if window is narrow.
Check if the `window-width' is less than `split-width-threshold'."
  (and (numberp split-width-threshold)
       (< (window-total-width) split-width-threshold)))

;;;###autoload
(defun lim-helper-three-or-more-windows-p (&optional frame)
  "Return non-nil if three or more windows occupy FRAME.
If FRAME is non-nil, inspect the current frame."
  (>= (length (window-list frame :no-minibuffer)) 3))

;;;###autoload
(defun lim-helper-line-regexp-p (type &optional n)
  "Test for TYPE on line.
TYPE is the car of a cons cell in
`lim--line-regexp-alist'.  It matches a regular
expression.

With optional N, search in the Nth line from point."
  (save-excursion
    (goto-char (line-beginning-position))
    (and (not (bobp))
         (or (beginning-of-line n) t)
         (save-match-data
           (looking-at
            (alist-get type lim-helper-line-regexp-alist))))))

;;;###autoload
(defun lim-helper-shell-command-with-exit-code-and-output (command &rest args)
  "Run COMMAND with ARGS.
Return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (buffer-string))))

;;;###autoload
(defun lim-helper-window-bounds ()
  "Return start and end points in the window as a cons cell."
  (cons (window-start) (window-end)))

;;;###autoload
(defun lim-helper-mode-id (&optional mode)
  "Return ID of mode.

example: (java-mode java-ts-mode) -> java."
  (s-replace "-ts" "" (s-replace "-mode" "" (symbol-name (or mode major-mode)))))

;;;###autoload
(defun lim-helper-plist-merge (&rest plists)
  "Create a single property list from all plists in PLISTS."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls ov)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls) ov (plist-get rtn p))
        (when (and (plistp v)
                   (not (null ov))
                   (plistp ov))
          (setq v (lim-helper-plist-merge ov v)))
        (setq rtn (plist-put rtn p v))))
    rtn))

;;;###autoload
(defun lim-helper-plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun lim-helper-load-options (filepath)
  (when (and (f-exists-p filepath) (f-file-p filepath))
    (with-temp-buffer
      (insert-file-contents (f-full filepath))
      (json-parse-buffer :object-type 'plist :false-object :json-false))))

(provide 'lim-helper)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
