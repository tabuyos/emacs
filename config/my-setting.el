(add-to-list 'load-path "~/.emacs.d/packages/window-numbering")

;; Use shift + arrow to mark region.
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

;; Setting transparent effect.
(global-set-key [(C-f11)] 'loop-alpha)
(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35) (55 25) (45 15) (35 10)))
(defun loop-alpha ()
	(interactive)
	(let ((h (car alpha-list)))                ;; head value will set to
		((lambda (a ab)
			 (set-frame-parameter (selected-frame) 'alpha (list a ab))
			 (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
			 ) (car h) (car (cdr h)))
		(setq alpha-list (cdr (append alpha-list (list h))))
		)
	)

;; Setting comment block.
(defun qiang-comment-dwim-line (&optional arg)
	(interactive "*P")
	(comment-normalize-vars)
	(if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
		(comment-or-uncomment-region (line-beginning-position) (line-end-position))
		(comment-dwim arg)))
(global-set-key "\M-;" 'qiang-comment-dwim-line)

;; Use C-w to cut line and M-w to copy line.
(defadvice kill-ring-save (before slickcopy activate compile)  
	(interactive  
		(if mark-active (list (region-beginning) (region-end))  
			(list (line-beginning-position)  
				(line-beginning-position 2)))))  
(defadvice kill-region (before slickcut activate compile)  
	(interactive  
		(if mark-active (list (region-beginning) (region-end))  
			(list (line-beginning-position)  
				(line-beginning-position 2)))))

;; Use M-0 M-1 ... M-9 move window.
(require 'window-numbering)
(window-numbering-mode 1)

;; Use C-j to generate new line.
(defun my-newline ()
	(interactive)
	(move-end-of-line 1)
	(newline-and-indent))
;; Similar to C-w
(defun my-delete-line ()
	(interactive)
	(kill-whole-line))

(defun refresh-file ()
	(interactive)
	(revert-buffer t (not (buffer-modified-p)) t))

(defun sudo-edit-current-file ()
	(interactive)
	(when (buffer-file-name)
		(let ((old-point (point)))
			(find-file (concat "/sudo:root@localhost:" (buffer-file-name)))
			(goto-char old-point))))

;; Setting Ctrl+f5 for refresh-file
(global-set-key [(control f5)] 'refresh-file)

(global-set-key [(control j)] 'my-newline) ;; Setting Ctrl+j for my-newline.
;; (define-key key-translation-map (kbd "C-j") (key-binding (kbd "C-e C-j"))) ;; NOT WORK! IDK.

(global-set-key (kbd "TAB") 'up-list) ;; Setting TAB to jump outside bracket.

(global-set-key [(control \;)] 'ace-jump-mode)

(provide 'my-setting)
