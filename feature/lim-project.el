;;; lim-project.el --- Lim's Project -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Project for Lim

;;; Code:

(require 'project)

(defvar lim-project-identifier ".cpr"
  "Project recognize identifiers.

CPR: Customize Project Root")

(defun lim-project-try-cpr (dir)
  "Determine if DIR is a non-VC project.
DIR must include a .cpr file to be considered a project."
  (if-let* ((root (locate-dominating-file dir lim-project-identifier))
            (name (file-name-nondirectory (directory-file-name root))))
      (list 'cpr name root)))

(cl-defmethod project-name ((project (head cpr)))
  "Return root directory of current PROJECT."
  (nth 1 project))

(cl-defmethod project-root ((project (head cpr)))
  "Return root directory of current PROJECT."
  (nth 2 project))

(defun lim-project-create ()
  "Create new project."
  (interactive))

(defun lim-project-switch ()
  "Switch other project."
  (interactive))

(defun lim-project-remove ()
  "Remove special project."
  (interactive))

(defun lim-project-open ()
  "Open CPR project."
  (interactive))

;;;###autoload
(defun lim-project-root ()
  "Return current project root directory."
  (project-root (project-current t)))

;;;###autoload
(defun lim-project-name ()
  "Return current project name."
  (project-name (project-current t)))

;;;###autoload
(defun lim-project-mark (dir)
  "Mark the project.

Will generate .cpr directory in DIR."
  (interactive
   (list (read-directory-name
          "Select Directory." nil (or (project-root (project-current t)) default-directory)
          (lambda (file)
            (or (file-directory-p file)
                (insert-directory-wildcard-in-dir-p
                 (file-name-as-directory (expand-file-name file))))))))
  (let* ((do (y-or-n-p (concat "Mark: " dir)))
         (exsit (file-exists-p (expand-file-name lim-project-identifier dir))))
    (if (and do (not exsit))
        (mkdir (expand-file-name lim-project-identifier dir)))))

;;;###autoload
(define-minor-mode lim-project-mode
  "Toggle `lim-project-mode'."
  :init-value nil
  :global nil
  :lighter nil
  (if lim-project-mode
      (progn
        (add-hook 'project-find-functions 'lim-project-try-cpr))
    (remove-hook 'project-find-functions 'lim-project-try-cpr)))

;;;###autoload
(define-globalized-minor-mode global-lim-project-mode lim-project-mode
  (lambda ()
    (unless (minibufferp)
      (lim-project-mode 1))))

(provide 'lim-project)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
