;;; lim-simple.el --- The Simple for Lim -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Lim's Simple Library.

;;; Code:

(require 'lim-helper)

(defun lim-simple--mark (bounds)
  "Mark between BOUNDS as a cons cell of beginning and end positions."
  (push-mark (car bounds))
  (goto-char (cdr bounds))
  (activate-mark))

(defun lim-simple--dont-move (fn &rest args)
  "Don't move position of current point."
  (let ((current-point (point)))
    (funcall fn args)
    (goto-char current-point)))

;;;###autoload
(defun lim-simple-switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

;;;###autoload
(defun lim-simple-mark-sexp ()
  "Mark symbolic expression at or near point.
Repeat to extend the region forward to the next symbolic
expression."
  (interactive)
  (if (and (region-active-p)
           (eq last-command this-command))
      (ignore-errors (forward-sexp 1))
    (when-let ((thing (cond
                       ((thing-at-point 'url) 'url)
                       ((thing-at-point 'sexp) 'sexp)
                       ((thing-at-point 'string) 'string)
                       ((thing-at-point 'word) 'word))))
      (lim-simple--mark (bounds-of-thing-at-point thing)))))

;;;###autoload
(defun lim-simple-compilation-buffer-name (&rest _)
  "Using `compile-command` as compilation buffer name."
  (let ((cc (s-truncate 80 compile-command))
        (pn (project-name (project-current t))))
    (concat "*" pn ":compilation:" cc "*")))

;; ;;;###autoload
;; (defun lim-simple-dired-sidebar-preview ())

;;;###autoload
(defun lim-simple-dired-sidebar-show-selected-file ()
  "Show selected file."
  (interactive)
  (if (dired-sidebar-showing-sidebar-p)
      (dired-sidebar-point-at-file
       (dired-sidebar-get-file-to-show)
       (dired-sidebar-get-dir-to-show))
    (dired-sidebar-toggle-sidebar)))

;;;###autoload
(defun lim-simple-whitespace-disable ()
  "Disable `whitespace-mode'."
  (interactive)
  (whitespace-mode -1))

;;;###autoload
(defun lim-simple-whitespace-enable ()
  "Enable `whitespace-mode'."
  (interactive)
  (whitespace-mode +1))

;;;###autoload
(defun lim-simple-deactivate-mark ()
  "Deactivate mark."
  (interactive)
  (deactivate-mark))

;;;###autoload
(defun lim-simple-insert-space ()
  "Insert space character."
  (interactive)
  (if isearch-mode
      (isearch-printing-char ? )
    (insert " ")))

;;;###autoload
(defun lim-simple-insert-comma ()
  "Insert comma character."
  (interactive)
  (if isearch-mode
      (isearch-printing-char ?,)
    (insert ",")))

;;;###autoload
(defun lim-simple-insert-comma-space ()
  "Insert comma-space characters."
  (interactive)
  (if isearch-mode
      (progn
        (isearch-printing-char ?,)
        (isearch-printing-char ? ))
    (insert ", ")))

;;;###autoload
(defun lim-simple-quit ()
  "Quit"
  (interactive)
  (if (minibufferp)
      (minibuffer-keyboard-quit)
    (keyboard-quit)))

;;;###autoload
(defun lim-simple-mc-mark-at-point ()
  "Push current point mark."
  (interactive)
  (mc/create-fake-cursor-at-point))

;;;###autoload
(defun lim-simple-mc-activate ()
  "Activate Multi Cursor."
  (interactive)
  (mc/maybe-multiple-cursors-mode))

;;;###autoload
(defun lim-simple-run-shell-command (command)
  "Run shell command."
  (let ((output-buffer (generate-new-buffer " *lim-sco*"))
        (error-buffer (generate-new-buffer " *lim-sce*"))
        (result))
    (shell-command command output-buffer error-buffer)
    (setq result (with-current-buffer output-buffer (buffer-string)))
    (kill-buffer output-buffer)
    (kill-buffer error-buffer)
    (if (length= result 0) nil result)))

;;;###autoload
(defun lim-simple-copy-line ()
  "Copy the current line to the `kill-ring'."
  (interactive)
  (copy-region-as-kill (line-beginning-position) (line-end-position)))

;;;###autoload
(defun lim-simple-duplicate-line-or-region (n)
  "Duplicate the current line or active region."
  (interactive "p")
  (if mark-active
      (lim-simple-duplicate-region n)
    (lim-simple-duplicate-line n)))

(defun lim-simple-duplicate-line (n)
  "Duplicate the current line."
  (let* ((origin (point))
         (beg (pos-bol))
         (end (pos-eol))
         (content (buffer-substring-no-properties beg end)))
    (save-excursion
      (goto-char end)
      (dotimes (_ n)
        (newline)
        (insert content)))
    (goto-char (+ origin (* (length content) n) n))))

(defun lim-simple-duplicate-region (n)
  "Duplicate the current region."
  (let* ((deactivate-mark)
         (beg (region-beginning))
         (end (region-end))
         (content (buffer-substring-no-properties beg end))
         (offset (* (length content) n)))
    (save-excursion
      (goto-char end)
      (dotimes (_ n)
        (insert content)))
    (lim-simple--mark (cons (+ beg offset) (+ end offset)))))

;;;###autoload
(defun lim-simple-kill-whole-line-or-region (&optional n)
  "Simple `kill-whole-line' version."
  (interactive "p")
  (if (use-region-p)
      (lim-simple-kill-lines-in-region (count-lines (region-beginning) (region-end)))
    (lim-simple-kill-whole-line n)))

(defun lim-simple-kill-lines-in-region (&optional n)
  "Kill lines involved in the region."
  (save-excursion
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (kill-whole-line (or n (setq n 1))))
  (back-to-indentation))

(defun lim-simple-kill-whole-line (&optional n)
  "Simple `kill-whole-line' version."
  (kill-whole-line (or n (setq n 1)))
  (back-to-indentation))

;;;###autoload
(defun lim-simple-yank-replace-line-or-region ()
  "Replace line or region with latest kill.
This command can then be followed by the standard
`yank-pop' (default is bound to \\[yank-pop])."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (line-beginning-position) (line-end-position)))
  (yank))

;;;###autoload
(defun lim-simple-multi-line-below ()
  "Move half a screen below."
  (interactive)
  (forward-line (floor (window-height) 2))
  (setq this-command 'scroll-up-command))

;;;###autoload
(defun lim-simple-multi-line-above ()
  "Move half a screen above."
  (interactive)
  (forward-line (- (floor (window-height) 2)))
  (setq this-command 'scroll-down-command))

;;;###autoload
(defun lim-simple-unfill-region-or-paragraph (&optional beg end)
  "Unfill paragraph or, when active, the region.
Join all lines in region delimited by BEG and END, if active,
while respecting any empty lines (so multiple paragraphs are not
joined, just unfilled).  If no region is active, operate on the
paragraph.  The idea is to produce the opposite effect of both
`fill-paragraph' and `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (if (use-region-p)
        (fill-region beg end)
      (fill-paragraph))))

(defun lim-simple--display-unsaved-buffers (buffers buffer-menu-name)
  "Produce buffer menu listing BUFFERS called BUFFER-MENU-NAME."
  (let ((old-buf (current-buffer))
        (buf (get-buffer-create buffer-menu-name)))
    (with-current-buffer buf
      (Buffer-menu-mode)
      (setq-local Buffer-menu-files-only nil
                  Buffer-menu-buffer-list buffers
                  Buffer-menu-filter-predicate nil)
      (list-buffers--refresh buffers old-buf)
      (tabulated-list-print))
    (display-buffer buf)))

(defun lim-simple--get-unsaved-buffers ()
  "Get list of unsaved buffers."
  (seq-filter
   (lambda (buffer)
     (and (buffer-file-name buffer)
          (buffer-modified-p buffer)))
   (buffer-list)))

;;;###autoload
(defun lim-simple-display-unsaved-buffers ()
  "Produce buffer menu listing unsaved file-visiting buffers."
  (interactive)
  (if-let ((unsaved-buffers (lim-simple--get-unsaved-buffers)))
      (lim-simple--display-unsaved-buffers unsaved-buffers "*Unsaved buffers*")
    (message "No unsaved buffers")))

(defun lim-simple-display-unsaved-buffers-on-exit (&rest _)
  "Produce buffer menu listing unsaved file-visiting buffers.
Add this as :before advice to `save-buffers-kill-emacs'."
  (when-let ((unsaved-buffers (lim-simple--get-unsaved-buffers)))
    (lim-simple--display-unsaved-buffers unsaved-buffers "*Unsaved buffers*")))

;;;###autoload
(defun lim-simple-copy-current-buffer-filepath ()
  "Add the current buffer's file path to the `kill-ring'."
  (declare (interactive-only t))
  (interactive)
  (if buffer-file-name
      (kill-new buffer-file-name)
    (user-error "%s is not associated with a file" (buffer-name (current-buffer)))))

;;;###autoload
(defun lim-simple-rename-file-and-buffer (name)
  "Apply NAME to current file and rename its buffer.
Do not try to make a new directory or anything fancy."
  (interactive
   (list (read-string "Rename current file: " (buffer-file-name))))
  (let ((file (buffer-file-name)))
    (if (vc-registered file)
        (vc-rename-file file name)
      (rename-file file name))
    (set-visited-file-name name t t)))

(defun lim-simple--buffer-major-mode-prompt ()
  "Prompt of `lim-simple-buffers-major-mode'.
Limit list of buffers to those matching the current
`major-mode' or its derivatives."
  (let ((read-buffer-function nil)
        (current-major-mode major-mode))
    (read-buffer
     (format "Buffer for %s: " major-mode)
     nil
     :require-match
     (lambda (pair) ; pair is (name-string . buffer-object)
       (with-current-buffer (cdr pair)
         (derived-mode-p current-major-mode))))))

;;;###autoload
(defun lim-simple-buffers-major-mode ()
  "Select BUFFER matching the current one's major mode."
  (interactive)
  (switch-to-buffer (lim-simple--buffer-major-mode-prompt)))

(defun lim-simple--buffer-vc-root-prompt ()
  "Prompt of `lim-simple-buffers-vc-root'."
  (let ((root (or (vc-root-dir)
                  (locate-dominating-file "." ".git")))
        (read-buffer-function nil))
    (read-buffer
     (format "Buffers in %s: " root)
     nil t
     (lambda (pair) ; pair is (name-string . buffer-object)
       (with-current-buffer (cdr pair) (string-match-p root default-directory))))))

;;;###autoload
(defun lim-simple-buffers-vc-root ()
  "Select buffer matching the current one's VC root."
  (interactive)
  (switch-to-buffer (lim-simple--buffer-vc-root-prompt)))

(provide 'lim-simple)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
