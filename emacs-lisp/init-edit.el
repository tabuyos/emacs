;;; -*- coding: utf-8 -*-
;;; init-edit.el
;;
;; @autor: tabuyos
;; @since: 1.0
;; @date: 2021-06-27
;;
;;; Commentary:
;;
;; This is initial configuration for edit.
;;
;;; Code:

;; MoveText-
;; Move some (select) text
(defun te/shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
	(save-excursion
	  (indent-rigidly (region-beginning) (region-end) distance)
	  (push-mark mark t t)
	  (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position) (line-end-position) distance)))

(defun te/shift-right (count)
  (interactive "p")
  (te/shift-text count))

(defun te/shift-left (count)
  (interactive "p")
  (te/shift-text (- count)))
;; -MoveText

;; CommentBlock-
(defun te/comment-dwim-line (&optional arg)
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "M-;") #'te/comment-dwim-line)
;; -CommentBlock

;; CutAndCopy-
(defun te/kill-ring-save@around (fn &rest args)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (current-line-non-whitespace-begin-position) (current-line-non-whitespace-end-position))))
  (apply fn args))

(defun te/kill-region@around (fn &rest args)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (apply fn args))

;; Delete line and don't add string into kill-ring.
;; (delete-region (line-beginning-position) (line-beginning-position 2))

;; Add around advice for copy or cut
(advice-add 'kill-ring-save :around #'te/kill-ring-save@around)
(advice-add 'kill-region :around #'te/kill-region@around)
;; Remove around advice for copy or cut
;; (advice-remove 'kill-ring-save #'te/kill-ring-save@around)
;; (advice-remove 'kill-region #'te/kill-region@around)

;; -CutAndCopy

;; OpLine-
(defun te/is-normal-string (str)
  "Determine whether the current character is a normal string."
  (not (or (null str) (equal "" str) (equal " " str) (equal "\n" str) (equal "\t" str))))

(defun te/get-one-char (position &optional is-negative)
  "Get one char by position."
  (let ((forward (1+ position)) (backward (1- position)))
    (if is-negative
	(if (> position 0)
	    (buffer-substring-no-properties backward position)
	  nil)
      (if (> position 0)
	  (buffer-substring-no-properties position forward)
	nil))))

(defun te/current-line-non-whitespace-position (&optional reverse)
  "Get begin or end position of current line with non-whitespace."
  (let ((begin (line-beginning-position)) (end (line-end-position)) (loop t) po)
    (while (and loop (< begin end))
      (if reverse
	  (if (te/is-normal-string (get-one-char end reverse))
	      (setq loop nil)
	    (decf end))
	(if (te/is-normal-string (get-one-char begin reverse))
	    (setq loop nil)
	  (incf begin))
	(setq postion begin)))
    (if loop
	(if reverse
	    (line-beginning-position)
	  (line-end-position))
      (if reverse
	  end
	begin))))

(defun te/current-line-non-whitespace-begin-position ()
  "Get begin position of current line with non-whitespace."
  (te/current-line-non-whitespace-position nil))

(defun te/current-line-non-whitespace-end-position ()
  "Get end position of current line with non-whitespace."
  (te/current-line-non-whitespace-position t))

(defun te/gen-new-line-in-below ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun te/des-current-line ()
  (interactive)
  (kill-whole-line))

;; Define keymap for some line function
(global-set-key [(control j)] #'te/gen-new-line-in-below)
; (global-set-key [(tab)] #'up-list)
;; -OpLine

;; OpFile-
(defun te/refresh-current-file ()
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))

(defun te/sudo-edit-current-file ()
  (interactive)
  (when (buffer-file-name)
    (let ((old-point (point)))
      (find-file (concat "/sudo:" user-login-name "@localhost:" (buffer-file-name)))
      (goto-char old-point))))

(defvar *te/unshifted-special-chars-layout*
  '(("1" "!")
    ("2" "@")
    ("3" "#")
    ("4" "$")
    ("5" "%")
    ("6" "^")
    ("7" "&")
    ("8" "*")
    ("9" "(")
    ("0" ")")
    ("!" "1")
    ("@" "2")
    ("#" "3")
    ("$" "4")
    ("%" "5")
    ("^" "6")
    ("&" "7")
    ("*" "8")
    ("(" "9")
    (")" "0")))

(defun te/mb-str-to-unibyte-char (str)
  "Translate first multibyte char in s to internal unibyte representation."
  (multibyte-char-to-unibyte (string-to-char str)))

(defun te/remap-keyboard (mapping)
  "Setup keyboard translate table using a list of pairwise key-mappings."
  (mapcar
   (lambda (mb-string-pair)
     (apply #'keyboard-translate
            (mapcar #'te/mb-str-to-unibyte-char mb-string-pair)))
   mapping))

(defun te/change-keyboard-map ()
  "Change my keyboard map."
  (interactive)
  (remap-keyboard *te/unshifted-special-chars-layout*))

;; Define keymap for some file function
(global-set-key [(control f5)] #'te/refresh-current-file)
;; -OpFile

;; ;; PanguSpacing-
;; ;; maybe freezing!!!
;; (use-package pangu-spacing
;;   :defer t
;;   :config
;;   (global-pangu-spacing-mode))
;; ;; -PanguSpacing

;; AceJump-
(use-package ace-jump-mode
  :ensure t
  :bind (("C-;" . ace-jump-mode)))
;; -AceJump

;; MoveText-
(use-package move-text
  :ensure t
  :bind
  (("C-c p" . move-text-up)
   ("C-c n" . move-text-down)))
;; -MoveText

;; UserProfile-
;; close backup file.
(setq make-backup-files nil)
;; open global display line number mode
(global-display-line-numbers-mode)
;; open line number mode.
(line-number-mode)
;; open column number mode.
(column-number-mode)
;; -UserProfile

(provide 'init-edit)
;;; init-edit.el ends here.
