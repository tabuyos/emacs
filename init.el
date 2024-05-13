;;; init.el --- The Emacs Initialization File Entry -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2023 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2023/10/05

;;; Commentary:

;; The Emacs Initialization Main File

;;; Code:

(defun neuron/update-to-load-path (folder)
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

(neuron/update-to-load-path (expand-file-name "elisp" user-emacs-directory))

(require 'init-constant)
(require 'init-package)
(require 'init-gui-frame)
(require 'init-edit)
(require 'init-theme)
(require 'init-font)

(require 'init-eglot)

;; (require 'init-haskell)
;; (require 'init-rust)

(require 'init-treemacs)

(require 'init-awesome-pair)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
