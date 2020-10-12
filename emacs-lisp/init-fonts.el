;;; package --- tabuyos-init --- init-fonts.el
;;; Commentary:
;; This is initial configuration for fonts.
;;; Code:

;; FontsList-
;; Input Mono, Monaco Style, Line Height 1.3 download from http://input.fontbureau.com/
(defvar default-font-size 11.0
  "Default size of font.")

(defvar chinese-font-list '(("Fandol") ("Noto Serif CJK SC") ("Noto Sans CJK SC"))
  "List of Chinese font, size isn't set by defualt.")

(defvar font-list '(("Noto Mono") ("Consolas" . 11.0)("Input" . 11.0) ("SF Mono" . 11.0) ("Consolas" . 11.0) ("Love LetterTW" . 11.0))
  "List of fonts and sizes.  The first one available will be used.")
;; -FontsList

;; FontFun-
(defun change-font ()
  "Documentation."
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
      (when (member (car font) (font-family-list))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t) available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting)))))

(when (display-graphic-p)
  (change-font))
;; -FontFun

;; ATIPac-
(use-package all-the-icons :if (display-graphic-p))
;; -ATIPac
(set-frame-font (font-spec :family "Noto Mono" :size 11.0))
(set-frame-font (font-spec :family "Consolas" :size 11.0))
;; 我是中文
(set-fontset-font "fontset-default" 'han (font-spec :family "FandolKai"))

(defun say-hello (name)
  "Hello to NAME."
  (interactive "sHello to somebady:")
  (message "Hello %s" name))

(completing-read "choice: " '(aadfd bfds afc bff eddd))
(read-number "size: " 3)
(read-string "size: " nil nil "tabuyos")

(let (font-name font-size)
  (defvar default-font-size 11.0
    "Default size of font.")

  (defvar available-font-list nil
    "List of available font.")

  (defvar chinese-font-list '("Fandol" "Noto Serif CJK SC" "Noto Sans CJK SC")
    "List of Chinese font, size isn't set by defualt(default size: `default-font-size').")

  (defvar default-font-list '("Noto Mono" "Consolas" "Input" "Consolas" "Love LetterTW" "Source Code Variable" "DejaVu Sans Code" "JetBrains Mono")
    "List of fonts and sizes.  The first one available will be used.")
  (setq available-font-list nil)
  (dolist (font-name default-font-list)
    (when (member font-name (font-family-list))
      (push font-name available-font-list)))
  (setq available-font-list (nreverse available-font-list))
  (print available-font-list)
  (if (not available-font-list)
      (message "No fonts available, check your font was add into fonts' folder and try again.")
    (if (not (called-interactively-p 'interactive))
	(setq font-name (assoc-string (completing-read "What font to use? " available-font-list) available-font-list)
	      font-size (read-number "Font size: " default-font-size))
      (setq font-name (car available-font-list)
	    font-size default-font-size)))
  (if (integerp font-size)
      (setq font-size (/ (* 3 font-size) 4.0)))
  (print font-size)
  (print font-name)
  (set-frame-font (font-spec :family font-name :size font-size) nil t)
  
  )
(integerp 1)
(floatp 1.0)

(if (not nil)
    (print 1)
  (print 2)
  (print 3)
  (print 4)
  (print 5))
(provide 'init-fonts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-fonts.el ends here
