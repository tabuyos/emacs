;;; lim-scratch.el --- Scratch buffers for editable major mode -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Set up a scratch buffer for an editable major mode.

;;; Code:

(require 'lim-helper)

(defvar lim-scratch--major-mode-history nil
  "Minibuffer history of `lim-scratch--major-mode-prompt'.")

(defvar lim-scratch-default-mode 'emacs-lisp-mode
  "Scratch buffer default major mode.")

(defun lim-scratch--scratch-list-modes ()
  "List known major modes."
  (let (symbols)
    (mapatoms
     (lambda (symbol)
       (when (and (functionp symbol)
                  (or (provided-mode-derived-p symbol 'text-mode)
                      (provided-mode-derived-p symbol 'prog-mode)))
         (push symbol symbols))))
    symbols))

(defun lim-scratch--insert-comment ()
  "Insert comment for major mode, if appropriate.
Insert a comment if `comment-start' is non-nil and the buffer is
empty."
  (when (and (lim-helper-empty-buffer-p) comment-start)
    (insert (format "Scratch buffer for: %s\n\n" major-mode))
    (goto-char (point-min))
    (comment-region (line-beginning-position) (line-end-position))))

(defun lim-scratch--prepare-buffer (region &optional mode)
  "Add contents to scratch buffer and name it accordingly.

REGION is added to the contents to the new buffer.

Use the current buffer's major mode by default.  With optional
MODE use that major mode instead."
  (let ((major (or mode major-mode)))
    (with-current-buffer (pop-to-buffer (format "*%s scratch*" major))
      (funcall major)
      (lim-scratch--insert-comment)
      (goto-char (point-max))
      (unless (string-empty-p region)
        (when (lim-helper-line-regexp-p 'non-empty)
          (insert "\n\n"))
        (insert region)))))

(defun lim-scratch--major-mode-prompt ()
  "Prompt for major mode and return the choice as a symbol."
  (intern
   (completing-read "Select major mode: "
                    (lim-scratch--scratch-list-modes)
                    nil
                    :require-match
                    nil
                    'lim-scratch--major-mode-history)))

(defun lim-scratch--capture-region ()
  "Capture active region, else return empty string."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    ""))

;;;###autoload
(defun lim-scratch-confirm-kill-buffer ()
  "Confirm before killing the *scratch* buffer."
  (interactive)
  (when (yes-or-no-p "*scratch* Don't want to be Killed, Confirm? ")
    (kill-buffer (current-buffer))))

;;;###autoload
(defun lim-scratch-buffer (&optional arg)
  "Produce a scratch buffer matching the current major mode.

With optional ARG as a prefix argument (\\[universal-argument]),
use `lim-scratch-default-mode'.

With ARG as a double prefix argument, prompt for a major mode
with completion.  Candidates are derivatives of `text-mode' or
`prog-mode'.

If region is active, copy its contents to the new scratch
buffer.

Buffers are named as *MAJOR-MODE scratch*.  If one already exists
for the given MAJOR-MODE, any text is appended to it."
  (interactive "P")
  (let ((region (lim-scratch--capture-region)))
    (pcase (prefix-numeric-value arg)
      (16 (lim-scratch--prepare-buffer region (lim-scratch--major-mode-prompt)))
      (4 (lim-scratch--prepare-buffer region lim-scratch-default-mode))
      (_ (lim-scratch--prepare-buffer region)))))

(provide 'lim-scratch)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
