;;; package --- tabuyos-init --- init-edit.el
;;; Commentary:
;; This is initial configuration for edit.
;;; Code:

;; MoveText-
;; Move some (select) text
(defun shift-text (distance)
  (print (use-region-p))
  (if (use-region-p)
      (let ((mark (mark)))
	(save-excursion
	  (indent-rigidly (region-beginning) (region-end) distance)
	  (push-mark mark t t)
	  (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position) (line-end-position) distance)))

(defun shift-right (count)
  (interactive "p")
  (print count)
(shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))
;; -MoveText

;; CommentBlock-
(defun tabuyos-comment-dwim-line (&optional arg)
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "M-;") #'tabuyos-comment-dwim-line)
;; -CommentBlock

;; CutAndCopy-
(defun kill-ring-save@around (fn &rest args)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (apply fn args))

(defun kill-region@around (fn &rest args)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (apply fn args))


;; Add around advice for copy or cut
(advice-add 'kill-ring-save :around #'kill-ring-save@around)
(advice-add 'kill-region :around #'kill-region@around)
;; Remove around advice for copy or cut
;; (advice-remove 'kill-ring-save #'kill-ring-save@around)
;; (advice-remove 'kill-region #'kill-region@around)

;; -CutAndCopy

;; OpLine-
(defun gen-new-line-in-below ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun des-current-line ()
  (interactive)
  (kill-whole-line))
;; Define keymap for some line function
(global-set-key [(control j)] #'gen-new-line-in-below)
; (global-set-key [(tab)] #'up-list)
;; -OpLine

;; OpFile-
(defun refresh-current-file ()
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))

(defun sudo-edit-current-file ()
  (interactive)
  (when (buffer-file-name)
    (let ((old-point (point)))
      (file-file (concat "/sudo:root@localhost:" (buffer-file-name)))
      (goto-char old-point))))
;; Define keymap for some file function
(global-set-key [(control f5)] #'refresh-current-file)
;; -OpFile

;; AceJump-
(use-package ace-jump-mode
  :ensure t
  :bind (("C-;" . ace-jump-mode)))
;; -AceJump

;; UserProfile-
(setq make-backup-files nil)
;; -UserProfile


;; (defun test-back-to-indentation ()
;;   "Move point to the first non-whitespace character on this line."
;;   (interactive "^")
;;   (beginning-of-line 1)
;;   (skip-syntax-forward " " (line-end-position))
;;   ;; Move back over chars that have whitespace syntax but have the p flag.
;;   (backward-prefix-chars))

;;    (skip-chars-forward " ") (beginning-of-line)
(defun my-skip ()
  (beginning-of-line)
  (let ((ps (skip-chars-forward " " (line-end-position))))
    (print ps)))
     (my-skip)

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
