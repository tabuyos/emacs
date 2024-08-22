;;; lim-dev.el --- Lim's Dev Language -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Dev Language for Lim

;;; Code:

(defvar lim-dev-go-tools-alist nil "Tools for Golang.")

(defun lim-dev-install-go-tools (&optional tools update)
  "Install or update go tools."
  (interactive
   (list lim-dev-go-tools-alist
         (< 1 (prefix-numeric-value current-prefix-arg))))
  (unless (executable-find "go")
    (user-error "Unable to find `go' in `exec-path'!"))

  (dolist (tool tools)
    (let ((pkg (symbol-name (car-safe tool)))
          (url (cdr-safe tool)))
      (when (or update (not (executable-find pkg)))
        (set-process-sentinel
         (start-process
          "go-tools" nil
          "go" "install" "-v" "-x" (concat url "@latest"))
         `(lambda (proc _)
            (let ((status (process-exit-status proc)))
              (if (= 0 status)
                  (message "Installed %s" ,pkg)
                (message "Failed to install %s: %d" ,pkg status)))
            ))))))

(provide 'lim-dev)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
