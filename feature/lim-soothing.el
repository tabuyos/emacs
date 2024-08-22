;;; lim-soothing.el --- Lim Soothing -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Soothing

;;; Code:

(require 'newcomment)

(defvar lim-soothing-comment-timestamp-format-concise "%F"
  "Specifier for date in `lim-soothing-comment-timestamp-keyword'.
Refer to the doc string of `format-time-string' for the available
options.")

(defvar lim-soothing-comment-timestamp-format-verbose "%F %T %z"
  "Like `lim-soothing-comment-timestamp-format-concise', but longer.")

(defvar lim-soothing-comment-keywords
  '("TODO" "NOTE" "REVIEW" "FIXME")
  "List of strings with keywords used by `lim-soothing-comment-timestamp-keyword'.")

(defvar lim-soothing-comment--keyword-hist '()
  "Minibuffer history of `lim-soothing-comment--keyword-prompt'.")

(defun lim-soothing-comment--keyword-prompt (keywords)
  "Prompt for candidate among KEYWORDS (per `lim-soothing-comment-timestamp-keyword')."
  (let ((def (car lim-soothing-comment--keyword-hist)))
    (completing-read
     (format "Select keyword [%s]: " def)
     keywords nil nil nil 'lim-soothing-comment--keyword-hist def)))

(defun lim-soothing-comment--format-date (verbose)
  "Format date using `format-time-string'.
VERBOSE has the same meaning as `lim-soothing-comment-timestamp-keyword'."
  (format-time-string
   (if verbose
       lim-soothing-comment-timestamp-format-verbose
     lim-soothing-comment-timestamp-format-concise)))

(defun lim-soothing-comment--timestamp (keyword &optional verbose)
  "Format string using current time and KEYWORD.
VERBOSE has the same meaning as `lim-soothing-comment-timestamp-keyword'."
  (format "%s %s: " keyword (lim-soothing-comment--format-date verbose)))

(defun lim-soothing-comment--format-comment (string)
  "Format comment STRING per `lim-soothing-comment-timestamp-keyword'.
STRING is a combination of a keyword and a time stamp."
  (concat comment-start
          (make-string comment-add (string-to-char comment-start))
          comment-padding
          string
          comment-end))

(defun lim-soothing-comment--maybe-newline ()
  "Call `newline' if current line is not empty.
Check `lim-soothing-comment-timestamp-keyword' for the rationale."
  (unless (looking-at "[\s\t]*$")
    (save-excursion (newline))))

;;;###autoload
(defun lim-soothing-comment (n)
  "Comment N lines, defaulting to the current one.
When the region is active, comment its lines instead."
  (interactive "p")
  (comment-normalize-vars)
  (cond ((looking-at "[\s-]*$") (comment-dwim nil))
        (t (comment-line n))))

;;;###autoload
(defun lim-soothing-comment-timestamp-keyword (keyword &optional verbose)
  "Add timestamped comment with KEYWORD."
  (interactive
   (list
    (lim-soothing-comment--keyword-prompt lim-soothing-comment-keywords)
    current-prefix-arg))
  (let ((string (lim-soothing-comment--timestamp keyword verbose))
        (beg (point)))
    (cond ((looking-at "[\s\t]*$")
           (insert (lim-soothing-comment--format-comment string)))
          ((eq beg (line-beginning-position))
           (insert (lim-soothing-comment--format-comment string))
           (indent-region beg (point))
           (lim-soothing-comment--maybe-newline))
          (t
           (comment-indent t)
           (insert (concat " " string))))))

;;;###autoload
(defun lim-soothing-smart-beginning-of-line ()
  "Move point to `beginning-of-line'. If repeat command it cycle
position between `back-to-indentation' and `beginning-of-line'."
  (interactive "^")
  (if (and (eq last-command 'lim-soothing-smart-beginning-of-line)
           (not (bolp)))
      (beginning-of-line)
    (back-to-indentation)))

;;;###autoload
(defun lim-soothing-newline-on-below ()
  "Move to the next line and then opens a line.

See also `newline-and-indent'."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;;;###autoload
(defun lim-soothing-newline-on-above ()
    "Move to the prev line and then opens a line.

See also `newline-and-indent'."
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (previous-line)
  (indent-according-to-mode))

;;;###autoload
(defun soothin-join-line (&optional arg beg end)
  "Join this line and next line, or join all lines in the region if it is active.

ee also `delete-indentation'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (cons (if (use-region-p) nil 1)
                (and (use-region-p)
                     (list (region-beginning) (region-end))))))
  (delete-indentation arg beg en))

(defun lim-soothing-marker-is-point-p (marker)
  "Test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun lim-soothing-push-mark-maybe ()
  "Push mark into `global-mark-ring' if mark head or tail is not current location"
  (if (not global-mark-ring) (error "global-mark-ring is empty")
    (unless (or (lim-soothing-marker-is-point-p (car global-mark-ring))
                (lim-soothing-marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))

;;;###autoload
(defun lim-soothing-backward-global-mark ()
  "Use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (lim-soothing-push-mark-maybe)
  (when (lim-soothing-marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark))

;;;###autoload
(defun lim-soothing-forward-global-mark ()
  "Hack `pop-global-mark' to go in reverse, pushing current point if not on ring."
  (interactive)
  (soothingpush-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (lim-soothing-marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))

(defun lim-soothing--enable ()
    "Enable Soothing."
  (message "enable soothing."))

(defun lim-soothing--disable ()
    "Disable Soothing."
  (message "disable soothing."))

(defvar lim-soothing-mode-map
  (let ((keymap (make-sparse-keymap)))
    keymap)
  "Keymap for Soothing.")

;;;###autoload
(define-minor-mode lim-soothing-mode
  "Soothing Minor Mode.

Provide key-binding of edit"
  :init-value nil
  :global nil
  :lighter " Oo"
  :keymap lim-soothing-mode-map)

;;;###autoload
(define-globalized-minor-mode global-lim-soothing-mode lim-soothing-mode
  (lambda ()
    (unless (minibufferp)
      (lim-soothing-mode 1))))

(provide 'lim-soothing)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
