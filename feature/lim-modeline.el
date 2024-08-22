;;; lim-modeline.el --- The Modeline for Lim's Appearance -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Lim's Modeline Module

;;; Code:

(declare-function nerd-icons-icon-for-mode "nerd-icons" (mode &rest arg-overrides))
(declare-function vc-git--symbolic-ref "vc-git" (file))
(declare-function vc-git-working-revision "vc-git" (file))
(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defface lim-modeline-indicator nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators (combines nicely with my `spacious-padding'
package).")

(defface lim-modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator').")

(defface lim-modeline-indicator-red-bg
  '((default :inherit (bold lim-modeline-indicator))
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  "Face for modeline indicators with a background.")

(defface lim-modeline-indicator-green
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#005f00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#73fa7f")
    (t :foreground "green"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator').")

(defface lim-modeline-indicator-green-bg
  '((default :inherit (bold lim-modeline-indicator))
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for modeline indicators with a background.")

(defface lim-modeline-indicator-yellow
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6f4000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f0c526")
    (t :foreground "yellow"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator').")

(defface lim-modeline-indicator-yellow-bg
  '((default :inherit (bold lim-modeline-indicator))
    (((class color) (min-colors 88) (background light))
     :background "#805000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ffc800" :foreground "black")
    (t :background "yellow" :foreground "black"))
  "Face for modeline indicators with a background.")

(defface lim-modeline-indicator-blue
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#00228a")
    (((class color) (min-colors 88) (background dark))
     :foreground "#88bfff")
    (t :foreground "blue"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator').")

(defface lim-modeline-indicator-blue-bg
  '((default :inherit (bold lim-modeline-indicator))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for modeline indicators with a background.")

(defface lim-modeline-indicator-magenta
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6a1aaf")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e0a0ff")
    (t :foreground "magenta"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator').")

(defface lim-modeline-indicator-magenta-bg
  '((default :inherit (bold lim-modeline-indicator))
    (((class color) (min-colors 88) (background light))
     :background "#6f0f9f" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#e3a2ff" :foreground "black")
    (t :background "magenta" :foreground "black"))
  "Face for modeline indicators with a background.")

(defface lim-modeline-indicator-cyan
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#004060")
    (((class color) (min-colors 88) (background dark))
     :foreground "#30b7cc")
    (t :foreground "cyan"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator').")

(defface lim-modeline-indicator-cyan-bg
  '((default :inherit (bold lim-modeline-indicator))
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for modeline indicators with a background.")

(defface lim-modeline-indicator-gray
  '((t :inherit shadow))
  "Face for modeline indicators (e.g. see my `notmuch-indicator').")

(defface lim-modeline-indicator-gray-bg
  '((default :inherit (bold lim-modeline-indicator))
    (((class color) (min-colors 88) (background light))
     :background "#808080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#a0a0a0" :foreground "black")
    (t :inverse-video t))
  "Face for modeline indicatovrs with a background.")

(defface lim-modeline-light
  '((t :weight light))
  "Face for modeline font weight: light")

(defface lim-modeline-indicator-hydra-status-face
  '((t :inherit lim-modeline-indicator-magenta))
  "Face for hydra status.")

(defsubst lim-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(defun lim-modeline--calc-face (&rest args)
  "Calc the face according to the selection state of the window."
  (if (mode-line-window-selected-p)
      (plist-get args :active)
    (plist-get args :inactive)))

(defun lim-modeline--check-hydra ()
  "Check hydra status."
  (and (bound-and-true-p lim-hydra-modeline-names)
       (mode-line-window-selected-p)
       (not (null (car lim-hydra-modeline-names)))))

(defun lim-modeline--calc-inherit-from-text (text)
  "Calc the `:inherit' property of the face in the text."
  (plist-get (get-text-property 0 'face text) :inherit))

(defun lim-modeline--major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((assoc major-mode nerd-icons-mode-icon-alist)
                     (nerd-icons-icon-for-mode major-mode))
                    ((derived-mode-p 'text-mode) (char-to-string #xF15C))
                    ((derived-mode-p 'prog-mode) (char-to-string #xE6B1))
                    ((derived-mode-p 'comint-mode) (char-to-string #xF120))
                    ((derived-mode-p 'compilation-mode) (char-to-string #xF085))
                    (t "X"))))
    (propertize indicator
                'face (lim-modeline--calc-face
                       :active (lim-modeline--calc-inherit-from-text indicator)
                       :inactive 'shadow))))

(defun lim-modeline--major-mode-name ()
  "Return current mode's name."
  (cond ((consp mode-name) (car-safe mode-name))
        ((stringp mode-name) mode-name)
        (t (capitalize (string-replace "-mode" "" (symbol-name major-mode))))))

(defun lim-modeline--major-mode-name-with-hydra ()
  "Return current mode's name and add hydra's status face."
  (if (lim-modeline--check-hydra)
      (propertize (lim-modeline--major-mode-name) 'face 'lim-modeline-indicator-hydra-status-face)
    (lim-modeline--major-mode-name)))

(defun lim-modeline--buffer-identification-face ()
  "Return appropriate face or face list for `lim-modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun lim-modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let ((rev (vc-working-revision file backend))
             (branch (or (vc-git--symbolic-ref file)
                         (substring rev 0 7))))
    (capitalize branch)))

(defun lim-modeline--git-diffstat (file)
  "Return shortened Git diff numstat for FILE."
  (when-let* ((output (shell-command-to-string (format "git diff --numstat %s" file)))
              (stats (split-string output "[\s\t]" :omit-nulls "[\s\f\t\n\r\v]+"))
              (added (nth 0 stats))
              (deleted (nth 1 stats)))
    (cond
     ((and (equal added "0") (equal deleted "0"))
      "")
     ((and (not (equal added "0")) (equal deleted "0"))
      (propertize (format "+%s" added) 'face 'shadow))
     ((and (equal added "0") (not (equal deleted "0")))
      (propertize (format "-%s" deleted) 'face 'shadow))
     (t
      (propertize (format "+%s -%s" added deleted) 'face 'shadow)))))

(defun lim-modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   "  "
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch 'face face)
   " "
   (lim-modeline--git-diffstat file)
   ))

(defun lim-modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (lim-modeline--vc-text file branch face))

(defvar lim-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun lim-modeline--vc-get-face (key)
  "Get face from KEY in `lim-modeline--vc-faces'."
  (alist-get key lim-modeline--vc-faces 'up-to-date))

(defun lim-modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (lim-modeline--vc-get-face (vc-state file backend)))

(defun lim-modeline--flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    ;; (when (cl-plusp count)
    ;;   (number-to-string count))
    (number-to-string count)))

(defmacro lim-modeline--flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "lim-modeline--flymake-%s" type)) ()
     (when-let ((count (lim-modeline--flymake-counter
                        ,(intern (format ":%s" type)))))
       (concat
        ;; (propertize ,indicator 'face 'shadow)
        ,indicator
        " "
        (propertize count 'face ',(or face type))
        " "
        ))))

(lim-modeline--flymake-type error (nerd-icons-mdicon "nf-md-close_circle_outline" :face 'error))
(lim-modeline--flymake-type warning (nerd-icons-mdicon "nf-md-alert_outline" :face 'warning))
(lim-modeline--flymake-type note (nerd-icons-mdicon "nf-md-information_outline" :face 'success) success)

;; buffer-status
(defvar-local lim-modeline-buffer-status
    '(:eval
      (cond (buffer-read-only
             (propertize (format " %s " (char-to-string #xE0A2))
                         'face (lim-modeline--calc-face
                                :active 'lim-modeline-indicator-gray
                                :inactive 'shadow)))
            ((buffer-modified-p)
             (propertize (format " %s " (char-to-string #x25C9))
                         'face (lim-modeline--calc-face
                                :active 'lim-modeline-indicator-red
                                :inactive 'shadow)))
            (t
             (propertize (format " %s " (char-to-string #x25CE))
                         'face (lim-modeline--calc-face
                                :active 'lim-modeline-indicator-green
                                :inactive 'shadow)))))
  "Render current bufffer status: `read-only', `modified' or `default'")
(put 'lim-modeline-buffer-status 'risky-local-variable t)

;; window-number
(defvar-local lim-modeline-window-number
    '(:eval
      (let ((num
             (cond
              ((bound-and-true-p ace-window-display-mode)
               (aw-update)
               (window-parameter (selected-window) 'ace-window-path))
              ((bound-and-true-p winum-mode)
               (setq winum-auto-setup-mode-line nil)
               (winum-get-number-string))
              ((bound-and-true-p window-numbering-mode)
               (window-numbering-get-number-string))
              (t ""))))
        (when (length> num 0) (format " %s " num)))))
(put 'lim-modeline-window-number 'risky-local-variable t)

;; kbd-macro
(defvar-local lim-modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'lim-modeline-indicator-blue))))
(put 'lim-modeline-kbd-macro 'risky-local-variable t)

;; narrow
(defvar-local lim-modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'lim-modeline-indicator-cyan))))
(put 'lim-modeline-narrow 'risky-local-variable t)

;; hydra
(defvar-local lim-modeline-hydra
    '(:eval
      (when (lim-modeline--check-hydra)
        (let ((name (car lim-hydra-modeline-names)))
          (cond ((stringp name)
               (propertize (concat " " name " ")
                           'face 'lim-modeline-indicator-hydra-status-face))
              ((fboundp name)
               (propertize (concat " " (name) " ")
                           'face 'lim-modeline-indicator-hydra-status-face)))
          ))))
(put 'lim-modeline-hydra 'risky-local-variable t)

;; remote-status
(defvar-local lim-modeline-remote-status
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ " 'face 'lim-modeline-indicator-red))))
(put 'lim-modeline-remote-status 'risky-local-variable t)

;; window-dedicated-status
(defvar-local lim-modeline-window-dedicated-status
    '(:eval
      (when (window-dedicated-p)
        (propertize " = " 'face 'lim-modeline-indicator-gray))))
(put 'lim-modeline-window-dedicated-status 'risky-local-variable t)

;; input-method
(defvar-local lim-modeline-input-method
    '(:eval
      (when current-input-method-title
        (propertize (format " %s " current-input-method-title)
                    'face 'lim-modeline-indicator-green))))
(put 'lim-modeline-input-method 'risky-local-variable t)

;; major-mode
(defvar-local lim-modeline-major-mode
    (list
     " "
     (propertize "%[" 'face 'lim-modeline-indicator-red)
     '(:eval
       (concat
        (lim-modeline--major-mode-indicator)
        "  "
        (lim-modeline--major-mode-name)
        ;; (lim-modeline--major-mode-name-with-hydra)
        ))
     (propertize "%]" 'face 'lim-modeline-indicator-red)
     ))
(put 'lim-modeline-major-mode 'risky-local-variable t)

;; minor-mode
(defvar-local lim-modeline-minor-mode
    '((:propertize (" -" minor-mode-alist) face (italic lim-modeline-light))))
(put 'lim-modeline-minor-mode 'risky-local-variable t)

;; cursor-position
(defvar-local lim-modeline-cursor-position
    '("  "
      (:eval (cond
              ((and line-number-mode column-number-mode) "%l:%c")
              (line-number-mode "L%l")
              (column-number-mode "C%c")
              (t "")))
      " %p"))
(put 'lim-modeline-cursor-position 'risky-local-variable t)

;; selection
(defvar-local lim-modeline-selection
    '(:eval
      (when mark-active
        (propertize
         (let* ((beg (region-beginning))
                (end (region-end))
                (lines (count-lines beg (min end (point-max)))))
           (concat
            "  "
            (cond ((bound-and-true-p rectangle-mark-mode)
                   (let ((cols (abs (- (lim-modeline-column end)
                                       (lim-modeline-column beg)))))
                     (format "%dx%dB" lines cols)))
                  ((> lines 1)
                   (format "%dC %dL" (- end beg) lines))
                  (t
                   (format "%dC" (- end beg))))))
         'face 'lim-modeline-indicator-cyan))))
(put 'lim-modeline-selection 'risky-local-variable t)

;; encoding
(defvar-local lim-modeline-encoding
    '(:eval
      (propertize
       (concat
        "  "
        ;; eol type
        (let ((eol (coding-system-eol-type buffer-file-coding-system)))
          (pcase eol
            (0 "LF ")
            (1 "CRLF ")
            (2 "CR ")
            (_ "")))
        ;; coding system
        (let* ((sys (coding-system-plist buffer-file-coding-system))
               (cat (plist-get sys :category))
               (sym (if (memq cat '(coding-category-undecided coding-category-utf-8))
                        'utf-8
                      (plist-get sys :name))))
          (upcase (symbol-name sym)))
        " "
        )
       'face '(italic lim-modeline-light)
       )
      )
  )
(put 'lim-modeline-encoding 'risky-local-variable t)

;; buffer-id
(defvar-local lim-modeline-buffer-id
    '(:eval
      (propertize (concat " " (buffer-name))
                  'face (lim-modeline--buffer-identification-face))))
(put 'lim-modeline-buffer-id 'risky-local-variable t)

;; buffer-id-with-breadcrumb
(defvar-local lim-modeline-buffer-id-with-breadcrumb
    '(:eval
      (unless (bound-and-true-p breadcrumb-local-mode) lim-modeline-buffer-id)))
(put 'lim-modeline-buffer-id-with-breadcrumb 'risky-local-variable t)

;; process
(defvar-local lim-modeline-process
    (list '(" " mode-line-process)))
(put 'lim-modeline-process 'risky-local-variable t)

;; vc
(defvar-local lim-modeline-vc
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  ((vc-git-registered file))
                  (branch (lim-modeline--vc-branch-name file backend))
                  (face (lim-modeline--vc-face file backend)))
        (lim-modeline--vc-details file branch face))))
(put 'lim-modeline-vc 'risky-local-variable t)

;; eglot
(defvar-local lim-modeline-eglot
    '(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        (list
         " "
         '(eglot--managed-mode eglot--mode-line-format)))))
(put 'lim-modeline-eglot 'risky-local-variable t)

;; flymake
(defvar-local lim-modeline-flymake
    '(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         " "
         ;; See the calls to the macro `lim-modeline--flymake-type'
         '(:eval (lim-modeline--flymake-error))
         '(:eval (lim-modeline--flymake-warning))
         '(:eval (lim-modeline--flymake-note))))))
(put 'lim-modeline-flymake 'risky-local-variable t)

;; misc-info
(defvar-local lim-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        (list
         " "
         mode-line-misc-info))))
(put 'lim-modeline-misc-info 'risky-local-variable t)

;; notmuch-indicator
(defvar-local lim-modeline-notmuch-indicator
    '(notmuch-indicator-mode
      (" "
       (:eval (when (mode-line-window-selected-p)
                notmuch-indicator--counters)))))
(put 'lim-modeline-notmuch-indicator 'risky-local-variable t)


(with-eval-after-load 'spacious-padding
  (defun lim/modeline-spacious-indicators ()
    "Set box attribute to `'lim-modeline-indicator' if spacious-padding is enabled."
    (if (bound-and-true-p spacious-padding-mode)
        (set-face-attribute 'lim-modeline-indicator nil :box t)
      (set-face-attribute 'lim-modeline-indicator nil :box 'unspecified)))

  ;; Run it at startup and then afterwards whenever
  ;; `spacious-padding-mode' is toggled on/off.
  (lim/modeline-spacious-indicators)

  (add-hook 'spacious-padding-mode-hook #'lim/modeline-spacious-indicators))

(provide 'lim-modeline)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
