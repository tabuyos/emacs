;;; -*- coding: utf-8 -*-
;;; init.el
;;
;; @autor: tabuyos
;; @since: 1.0
;; @date: 2021-06-26
;;
;;; Commentary:
;;
;; This is initialize configuration file for emacs.
;;
;;; Code:

;; CheckVersion-
(defun te/require-early-init-config ()
  "require the early initial configuration file before loading the init.el file."
  (let* ((early-init-file (expand-file-name "early-init.el" user-emacs-directory))
	 (early-init-do-not-edit-directory (expand-file-name "early-init-do-not-edit/" user-emacs-directory))
	 (early-init-do-not-edit-file (expand-file-name "early-init.el" early-init-do-not-edit-directory)))
    (and (version< emacs-version "27")
	 (or (not (file-exists-p early-init-do-not-edit-file))
	     (file-newer-than-file-p early-init-file early-init-do-not-edit-file)))
    (make-directory early-init-do-not-edit-directory t)
    (copy-file early-init-file early-init-do-not-edit-file t t t t)
    (add-to-list 'load-path early-init-do-not-edit-directory)
    (require 'early-init)))

(defun te/check-emacs-version ()
  "check emacs version and load some configuration file."
  (cond ((version< emacs-version "27")
	 (warn "Tabuyos-Emacs requires Emacs 27 or above!"))
	(t (te/require-early-init-config))))

(te/check-emacs-version)
;; -CheckVersion

;; BetterGC-
(defvar te/better-gc-cons-threshold 268435456
  "The default value is 256MB.
If you experience freezing, decrease this, otherwise increase it.")

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold te/better-gc-cons-threshold)
            (setq file-name-handler-alist te/file-name-handler-alist-original)
            (makunbound 'te/file-name-handler-alist-original)))
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
	    (defun te/gc-minibuffer-setup-hook ()
	      (setq gc-cons-threshold (* te/better-gc-cons-threshold 2)))
            (defun te/gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold te/better-gc-cons-threshold))
            (add-hook 'minibuffer-setup-hook #'te/gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'te/gc-minibuffer-exit-hook)))
;; -AutoGC

;; LoadPath-
(defun te/update-to-load-path (folder)
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

(te/update-to-load-path (expand-file-name "emacs-lisp" user-emacs-directory))
;; -LoadPath

;; LoadModules-
(require 'init-constant)

(require 'init-package)

(require 'init-global-config)

(require 'init-frame)

(require 'init-edit)

(require 'init-function)

(require 'init-yasnippet)

(require 'init-company)

(require 'init-theme)

(require 'init-fonts)

;; Programing Language
(require 'init-awesome-pair)

(require 'init-programming)

(require 'init-go)
;; -LoadModules

;;; init.el ends here.
