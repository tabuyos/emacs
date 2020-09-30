;;; package --- tabuyos-init --- init.el
;;; Commentary:
;; This is initial configuration file for Emacs.
;;; Code:

;; CheckVersion-
(cond ((version< emacs-version "26.1")
       (warn "Taboo-Emacs requires Emacs 26.1 or above1"))
      ((let* ((early-init-file (expand-file-name "early-init.el" user-emacs-directory))
              (early-init-do-not-edit-directory (expand-file-name "early-init-do-not-edit/" user-emacs-directory))
              (early-init-do-not-edit-file (expand-file-name "early-init.el" early-init-do-not-edit-directory)))
         (and (version< emacs-version "27")
              (or (not (file-exists-p early-init-do-not-edit-file))
                  (file-newer-than-file-p early-init-file early-init-do-not-edit-file)))
         (make-directory early-init-do-not-edit-directory t)
         (copy-file early-init-file early-init-do-not-edit-file t t t t)
         (add-to-list 'load-path early-init-do-not-edit-directory)
         (require 'early-init))))
;; -CheckVersion

;; BetterGC-
(defvar better-gc-cons-threshold 268435456
  "The default value is 256MB.
If you experience freezing, decrease this, otherwise increase it.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
;; -BetterGC

;; AutoGC-
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))
            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))
            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -AutoGC

;; LoadPath-
(defun update-to-load-path (folder)
  "Update FOLDER and it's subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "emacs-lisp" user-emacs-directory))
;; -LoadPath

(require 'init-constant)

(require 'init-package)

(require 'init-global-config)

(require 'init-function)

(require 'init-theme)

; Programming
(require 'init-latex)

(require 'init-cc)

(require 'init-haskell)

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
