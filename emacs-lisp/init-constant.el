;;; -*- coding: utf-8 -*-
;;; init-constant.el
;;
;; @autor: tabuyos
;; @since: 1.0
;; @date: 2021-06-27
;;
;;; Commentary:
;;
;; This initializes constants
;;
;;; Code:

;; UserInfo-
(defvar te/user-nick-name nil
  "User's nickname for current user.")

(setq user-full-name "Aaron Liew")
(setq te/user-nick-name "Tabuyos")
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

(defconst *sys/python*
  (executable-find "python")
  "Do we have python?")

(defconst *sys/pip*
  (executable-find "pip")
  "Do we hava pip?")

(defconst *sys/clangd*
  (executable-find "clangd")
  "Do we have clangd?")

(defconst *sys/eaf-env*
  (and *sys/linux* (display-graphic-p) *sys/python* *sys/pip*
       (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") "")))
  "Do we have EAF environment setup?")
;; -Constants


(provide 'init-constant)
;;; init-constant.el ends here.
