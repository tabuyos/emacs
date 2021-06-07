;;; package --- tabuyos-init --- init-fonts.el
;;; Commentary:
;; This is initial configuration for fonts.
;;; Code:

;; FontsList-
;; Input Mono, Monaco Style, Line Height 1.3 download from http://input.fontbureau.com/
(defvar default-font-size 11.0
  "Default size of font.")

(defvar available-chinese-font-list nil
  "List of available font for chinese.")

(defvar available-english-font-list nil
  "List of available font for english.")

(defvar chinese-font-list '("FandolKai" "Noto Serif CJK SC" "Noto Sans CJK SC")
  "List of Chinese font, size isn't set by defualt(default size: `default-font-size').")

(defvar default-font-list '("Noto Mono" "Consolas" "Input" "Consolas" "Love LetterTW" "Source Code Variable" "DejaVu Sans Code" "JetBrains Mono")
  "List of fonts and sizes.  The first one available will be used.")
;; -FontsList

;; FontFun-
(defun check-available-font (pending-check-font-list)
  (let (available-font-list)
    (dolist (font-name pending-check-font-list)
      (when (member font-name (font-family-list))
	(push font-name available-font-list)))
    (setq available-font-list (nreverse available-font-list))))

(defun change-font ()
  "Documentation."
  (interactive)
  (let* (english-font-name chinese-font-name font-size ignore-case)
    (setq ignore-case completion-ignore-case
	  completion-ignore-case t)
    ;; check available font of english
    (when (not available-english-font-list)
      (setq available-english-font-list (check-available-font default-font-list)))
    ;; check available font fo chinese
    (when (not available-chinese-font-list)
      (setq available-chinese-font-list (check-available-font chinese-font-list)))

    (if (not available-english-font-list)
	(message "No english fonts available, check your font was add into fonts' folder and try again.")
      (if (called-interactively-p 'interactive)
	  (setq english-font-name (assoc-string (completing-read "What english font to use? " available-english-font-list (lambda (x) x)) available-english-font-list)
		chinese-font-name (assoc-string (completing-read "What chinese font to use? " available-chinese-font-list (lambda (x) x)) available-chinese-font-list)
		font-size (read-number "Font size: " default-font-size))
	(setq english-font-name (car available-english-font-list)
	      chinese-font-name (car available-chinese-font-list)
	      font-size default-font-size)))
    (setq completion-ignore-case ignore-case)
    (if (integerp font-size)
	(setq font-size (/ (* 3 font-size) 4.0)))
    (set-frame-font (font-spec :family english-font-name :size font-size) nil t)
    (set-fontset-font "fontset-default" 'han (font-spec :family chinese-font-name))))

(when (or (daemonp)(display-graphic-p))
  (change-font))
;; -FontFun

;; ATIPac-
(use-package all-the-icons :if (display-graphic-p))
;; -ATIPac

(provide 'init-fonts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-fonts.el ends here
