;;; package --- tabuyos-init
;;; Commentary:
;; This initializes constants
;;; Code:

;; UserInfo-
(defvar user-nick-name nil
  "User's nickname for current user.")

(setq user-full-name "Aaron Liew")
(setq user-nick-name "Tabuyos")
(setq user-mail-address "tabuyos@outlook.com")
;; -UserInfo

;; Constants-
(defconst *sys/windows*
  (eq system-type 'windows-nt)
  "Are we running on a Windows system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst python-p
  (executable-find "python")
  "Do we have python?")

(defconst pip-p
  (executable-find "pip")
  "Do we hava pip?")

(defconst clangd-p
  (executable-find "clangd")
  "Do we have clangd?")

(defconst eaf-env-p
  (and *sys/linux* (display-graphic-p) python-p pip-p
       (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") "")))
  "Do we have EAF environment setup?")
;; -Constants


(provide 'init-constant)
;;; init-constant.el ends here
