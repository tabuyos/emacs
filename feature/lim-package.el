;;; lim-package.el --- The Package Manager for Lim -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Lim's Package Manager Library.
;; Base on use-package

;;; Code:

(defvar tsinghua-source
  '(("gnu-tsinghua" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ("gnu-devel-tsinghua" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu-devel/")
    ("melpa-tsinghua" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ("stable-melpa-tshinghua" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
    ("nongnu-tsinghua" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
    ("nongnu-devel-tsinghua" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu-devel/"))
  "Using TsingHua University Source.")

(defvar tencent-source
  '(("gnu-tencent" . "http://mirrors.cloud.tencent.com/elpa/gnu/")
    ("melpa-tencent" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
    ("stable-melpa-tencent" . "http://mirrors.cloud.tencent.com/elpa/stable-melpa/")
    ("org-tencent" . "http://mirrors.cloud.tencent.com/elpa/org/"))
  "Using Tencent Cloud Source.")

(defvar emacs-china-source
  '(("gnu-emacs-china" . "http://elpa.emacs-china.org/gnu/")
    ("melpa-emacs-china" . "http://elpa.emacs-china.org/melpa/")
    ("stable-melpa-emacs-china" . "http://elpa.emacs-china.org/stable-melpa/")
    ("org-emacs-china" . "http://elpa.emacs-china.org/org/"))
  "Using Emacs China Source.")

(defvar tsinghua-source-priorities
  '(("gnu-tsinghua" . 3)
    ("melpa-tsinghua" . 2)
    ("nongnu-tsinghua" . 1))
  "Using TsingHua University Source Priorities")

(defvar tencent-source-priorities
  '(("gnu-tencent" . 3)
    ("melpa-tencent" . 2)
    ("org-tencent" . 1))
  "Using Tencent Cloud Source Priorities")

(defvar emacs-china-source-priorities
  '(("gnu-emacs-china" . 3)
    ("melpa-emacs-china" . 2)
    ("org-emacs-china" . 1))
  "Using Emacs China Source Priorities")

(defvar quelpa-use-package-repo-url
  "https://github.com/quelpa/quelpa-use-package.git"
  "This URL is the repo or quelpa-use-package.")

(defvar quelpa-download-url
  "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el"
  "This URL is for downloading Quelpa.")

(defvar straight-download-url
  "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
  "This URL is for downloading Quelpa.")

(defvar elpaca-installer-version 0.7)

(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))

(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))

(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))

(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

(defvar archives-dir
  (expand-file-name "elpa" user-emacs-directory)
  "User's package directory, default value is `package-user-dir'.")

(defvar archives-source
  tsinghua-source
  "User's customize archives source, default value is `tsinghua-source'.")

(defvar archives-priorities
  tsinghua-source-priorities
  "User's customize archives priorities, default value is `tsinghua-source-priorities'.")

(unless (bound-and-true-p bootstrap-version)
  (defvar bootstrap-version nil
    "Straight Bootstrap Version."))

(unless (bound-and-true-p package-user-dir)
  (defvar package-user-dir nil
    "package user's directory for elpa."))

(unless (bound-and-true-p package-archives)
  (defvar package-archives nil
    "package archives address."))

;;;###autoload
(defun integration-diminish ()
  "Integration Diminish."
  (use-package diminish))

;;;###autoload
(defun integration-el-get ()
  "Integration El Get."
  (use-package el-get))

;;;###autoload
(defun integration-quelpa ()
  "Integration Quelpa."
  (use-package quelpa
    :hook
    ((after-init . (lambda () (quelpa-upgrade-all-maybe))))
    :config
    (setq quelpa-upgrade-interval 7)
    (use-package quelpa-use-package)
    ;; enable advice
    (quelpa-use-package-activate-advice)))

;;;###autoload
(defun integration-straight ()
  "Integration Straight."
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-temp-buffer
        (url-insert-file-contents straight-download-url)
        (eval-buffer)))
    (load bootstrap-file nil 'nomessage)))

;;;###autoload
(defun integration-elpaca ()
  "Integration Elpaca."
  (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
         (build (expand-file-name "elpaca/" elpaca-builds-directory))
         (order (cdr elpaca-order))
         (default-directory repo))
    (add-to-list 'load-path (if (file-exists-p build) build repo))
    (unless (file-exists-p repo)
      (make-directory repo t)
      (when (< emacs-major-version 28) (require 'subr-x))
      (condition-case-unless-debug err
          (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                   ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                   ,@(when-let ((depth (plist-get order :depth)))
                                                       (list (format "--depth=%d" depth) "--no-single-branch"))
                                                   ,(plist-get order :repo) ,repo))))
                   ((zerop (call-process "git" nil buffer t "checkout"
                                         (or (plist-get order :ref) "--"))))
                   (emacs (concat invocation-directory invocation-name))
                   ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                         "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                   ((require 'elpaca))
                   ((elpaca-generate-autoloads "elpaca" repo)))
              (progn (message "%s" (buffer-string)) (kill-buffer buffer))
            (error "%s" (with-current-buffer buffer (buffer-string))))
        ((error) (warn "%s" err) (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
      (require 'elpaca)
      (elpaca-generate-autoloads "elpaca" repo)
      (load "./elpaca-autoloads")))
  (add-hook 'after-init-hook #'elpaca-process-queues)
  (elpaca `(,@elpaca-order))

  (elpaca elpaca-use-package (elpaca-use-package-mode 1)))

(provide 'lim-package)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
