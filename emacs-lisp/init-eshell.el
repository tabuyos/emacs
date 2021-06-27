;;; package --- tabuyos-init --- init-eshell.el
;;; Commentary:
;; This configuration for tabuyos
;;; Code:

;; MyEshell-
(defun eshell-life-is-too-much@after ()
  "Kill windows."
  (when (not (one-window-p))
    (delete-window)))

(advice-add 'eshell-life-is-too-much :after 'eshell-life-is-too-much@after)

(eval-when-compile
  (defvar eshell-mode-map nil))

(defun tabuyos-eshell-mode-hook ()
  "My customized eshell mode hook."
  (define-key eshell-mode-map (kbd "C-d") #'close-buffer-or-delete-char))

(eval-when-compile
  (defcustom eshell-prompt-regexp "^[^#$\n]* [#$] "
    "A regexp which fully matches your eshell prompt.
This setting is important, since it affects how eshell will interpret
the lines that are passed to it.
If this variable is changed, all Eshell buffers must be exited and
re-entered for it to take effect."
    :type 'regexp
    :group 'eshell-prompt))

(defun close-buffer-or-delete-char (&optional arg)
  "Close ehsell buffer if non-char else delete char with ARG."
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp nil))
      (eshell-life-is-too-much)
    (delete-char arg)))

(defun eshell/clear ()
  "Clear eshell buffer."
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

(add-hook 'eshell-mode-hook #'tabuyos-eshell-mode-hook)
;; -MyEshell

(provide 'init-eshell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eshell.el ends here
