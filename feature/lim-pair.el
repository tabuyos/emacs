;;; lim-pair.el --- Insert character pair around symbol or region -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Insert character pair around symbol or region.

;;; Code:

(defvar lim-pair-pairs
  '((?'  :description "Single quotes"           :pair (?' . ?'))
    (?\" :description "Double quotes"           :pair (?\" . ?\"))
    (?\( :description "Parentheses"             :pair (?\( . ?\)))
    (?{  :description "Curly brackets"          :pair (?{ . ?}))
    (?\[ :description "Square brackets"         :pair (?\[ . ?\]))
    (?\< :description "Angled brackets"         :pair (?\< . ?\>))
    (?@  :description "At signs"                :pair (?@ . ?@))
    (?=  :description "Equals signs"            :pair (?= . ?=))
    (?+  :description "Plus signs"              :pair (?+ . ?+))
    (?`  :description "Backticks"               :pair lim-pair-insert-backticks)
    (?~  :description "Tildes"                  :pair (?~ . ?~))
    (?*  :description "Asterisks"               :pair (?* . ?*))
    (?/  :description "Forward slashes"         :pair (?/ . ?/))
    (?_  :description "Underscores"             :pair (?_ . ?_)))
  "Alist of pairs for use with `lim-pair-insert'.
Each element in the list is a list whose `car' is a character and
the `cdr' is a plist with a `:description' and `:pair' keys.  The
`:description' is a string used to describe the character/pair in
interactive use, while `:pair' is a cons cell referencing the
opening and closing characters.

The value of `:pair' can also be the unquoted symbol of a
function.  The function is called with no arguments and must
return a cons cell of two characters.  Examples of such functions
are `lim-pair-insert-natural-language-quotes' and
`lim-pair-insert-backticks'")

(defvar lim-pair--insert-history nil
  "Minibuffer history of `lim-pair--insert-prompt'.")

(defun lim-pair-insert-backticks ()
  "Return pair of backticks for `lim-pair-pairs'.
When the major mode is derived from `lisp-mode', return a pair of
backtick and single quote, else two backticks."
  (if (derived-mode-p 'lisp-mode 'lisp-data-mode)
      (cons ?` ?')
    (cons ?` ?`)))

(defun lim-pair--annotate (character)
  "Annotate CHARACTER with its description in `lim-pair-pairs'."
  (when-let ((char (if (characterp character) character (string-to-char character)))
             (plist (alist-get char lim-pair-pairs))
             (description (plist-get plist :description)))
    (format "  %s" description)))

(defun lim-pair--get-pair (character)
  "Get the pair of corresponding to CHARACTER."
  (when-let ((char (if (characterp character) character (string-to-char character)))
             (plist (alist-get char lim-pair-pairs))
             (pair (plist-get plist :pair)))
    pair))

(defun lim-pair--insert-prompt ()
  "Prompt for pair among `lim-pair-pairs'."
  (let ((default (car lim-pair--insert-history))
        (candidates (mapcar (lambda (char) (char-to-string (car char))) lim-pair-pairs))
        (completion-extra-properties `(:annotation-function ,#'lim-pair--annotate)))
    (completing-read
     (format-prompt "Select pair" default)
     candidates nil :require-match
     nil 'lim-pair--insert-history default)))

(defun lim-pair--insert-bounds ()
  "Return boundaries of symbol at point or active region."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if (null bounds)
          (cons (point) (point))
        bounds))))

(defun lim-pair--insert-pair (pair n)
  "Insert N number of PAIR around object or direct at point."
  (let* ((bounds (lim-pair--insert-bounds))
         (point (point))
         (beg (car bounds))
         (end (1+ (cdr bounds)))
         (characters (if (functionp pair) (funcall pair) pair)))
    (dotimes (_ n)
      (save-excursion
        (goto-char beg)
        (insert (car characters))
        (goto-char end)
        (setq end (1+ end))
        (insert (cdr characters))))
    (goto-char (+ point n))))

;;;###autoload
(defun lim-pair-insert (pair n)
  "Insert N number of PAIR around object or direct at point.
PAIR is one among `lim-pair-pairs'.  The object at point is
either a symbol or the boundaries of the active region.  N is a
numeric prefix argument, defaulting to 1 if none is provided in
interactive use."
  (interactive
   (list
    (lim-pair--get-pair (lim-pair--insert-prompt))
    (prefix-numeric-value current-prefix-arg)))
  (lim-pair--insert-pair pair n))

;;;###autoload
(defun lim-pair-quick-insert (n)
  "Quick insert N number of PAIR around object or direct at point."
  (interactive "p")
  (lim-pair--insert-pair (lim-pair--get-pair last-input-event) n))

(defvar lim-pair-mode-map
  (let ((keymap (make-sparse-keymap)))
    (mapcar (lambda (entry) (keymap-set keymap (char-to-string (car entry)) #'lim-pair-quick-insert)) lim-pair-pairs)
    keymap))

;;;###autoload
(define-minor-mode lim-pair-mode
  "Pair minor mode."
  :init-value nil
  :global nil
  :keymap lim-pair-mode-map)

;;;###autoload
(define-globalized-minor-mode global-lim-pair-mode lim-pair-mode
  (lambda ()
    (unless (minibufferp)
      (lim-pair-mode 1))))

(provide 'lim-pair)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
