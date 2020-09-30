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

(defun resize-window (width delta)
  "Resize the cureent window's size.  If WIDTH is non-nil, resize witdth by some DELTA."
  (if (> (count-windows) 1)
      (window-resize nil delta width)
    (error "You need more than 1 window to execute this function!")))

;; Setup shorcuts for window resize width and height.
(global-set-key (kbd "M-W =") (lambda () (interactive) (resize-window t 5)))
(global-set-key (kbd "M-W M-+") (lambda () (interactive) (resize-window t 5)))
(global-set-key (kbd "M-W -") (lambda () (interactive) (resize-window t -5)))
(global-set-key (kbd "M-W M-_") (lambda () (interactive) (resize-window t -5)))

(global-set-key (kbd "M-H =") (lambda () (interactive) (resize-window nil 5)))
(global-set-key (kbd "M-H M-+") (lambda () (interactive) (resize-window nil 5)))
(global-set-key (kbd "M-H -") (lambda () (interactive) (resize-window nil -5)))
(global-set-key (kbd "M-H M-_") (lambda () (interactive) (resize-window nil -5)))
;; -ResizeWindowHeight

;; EditConfig-
(defun edit-configs ()
  "Opens the README.org file."
  (interactive)
  (find-file "~/.emacs.d/init.org"))

(global-set-key (kbd "C-z e") #'edit-configs)
;; -EditConfig

;; OrgIncludeAuto-
(defun save-and-update-includes ()
  "Update the line numbers of #+INCLUDE:s in current buffer.
Only looks at INCLUDEs that have eigher :range-begin or :range-end.
This function does nothing if not in `org-mode', so you can safely add it to `before-save-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\INCLUDE: *\"\\([^\"]+\\)\".*:range-\\(begin\\|end\\)" nil 'noerror)
        (let* ((file (expand-file-name (match-string-no-properties 1)))
               lines begin end)
          (forward-line 0)
          (when (looking-at "^.*:range-begin *\"\\([^\"]+\\)\"")
            (setq begin (match-string-no-properties 1)))
          (when (looking-at "^.*:range-end *\"\\([^\"]+\\)\"")
            (setq end (match-string-no-properties 1)))
          (setq lines (decide-line-range file begin end))
          (when lines
            (if (looking-at ".*:lines *\"\\([-0-9]+\\)\"")
                (replace-match lines :fixedcase :literal nil 1)
              (goto-char (line-end-position))
              (insert " :lines \"" lines "\""))))))))

(add-hook 'before-save-hook #'save-and-update-includes)

(defun decide-line-range (file begin end)
  "Visit FILE and decide which lines to include.
BEGIN and END are regexps which define the line range to use."
  (let (l r)
    (save-match-data
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (if (null begin)
            (setq l "")
          (search-forward-regexp begin)
          (setq l (line-number-at-pos (match-beginning 0))))
        (if (null end)
            (setq r "")
          (search-forward-regexp end)
          (setq r (1+ (line-number-at-pos (match-end 0)))))
        (format "%s-%s" (+ l 1) (- r 1)))))) ;; Exclude wrapper
;; -OrgIncludeAuto

;; BetterMiniBuffer-
(defun abort-minibuffer-using-mouse ()
  "Abort the minibuffer when using the mouse."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'abort-minibuffer-using-mouse)

;; keep the point out of the minibuffer
(setq-default minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
;; -BetterMinibuffer

;; DisplayLineOverlay-
(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.
FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beinning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:background null :inherit highlight)))
    ol))
;; -DispalyLineOverlay

;; ReadLines-
(defun read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer (insert-file-contents file-path)
                    (split-string (buffer-string) "\n" t)))
;; -ReadLines

;; WhereAmI-
(defun where-am-i ()
  "An interactive function showing function `buffer-file-name' or `buffer-name'."
  (interactive)
  (message (kill-new (if (buffer-file-name) (buffer-file-name) (buffer-name)))))
;; -WhereAmI

(provide 'init-function)
;;; init-function.el ends here
