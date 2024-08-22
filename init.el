(defun lim-loading-paths (&rest folders)
  (dolist (folder folders)
    (when (file-directory-p folder)
      (let ((parent folder))
        (unless (member parent load-path)
          (add-to-list 'load-path parent))
        (dolist (name (remove ".." (remove "." (directory-files parent))))
          (let ((child (expand-file-name name parent)))
            (when (file-directory-p child)
              (unless (member child load-path)
                (lim-loading-paths child))))))
      )))

(defvar lim-module-dir
  (expand-file-name "module" user-emacs-directory)
  "Lim's initilize directory.")

(defvar lim-feature-dir
  (expand-file-name "feature" user-emacs-directory)
  "Lim's feature directory.")

(defvar lim-customize-file
  (expand-file-name "customizes.el" user-emacs-directory)
  "Lim's customize file.")

(lim-loading-paths lim-module-dir lim-feature-dir)

;; (use-package benchmark-init
;;   :hook
;;   (after-init . benchmark-init/deactivate))

;; (benchmark-init/activate)

;; Enable module
(require 'lim-module-base)
(require 'lim-module-package)
(require 'lim-module-icon)
(require 'lim-module-appearance)
(require 'lim-module-essential)
(require 'lim-module-hydra)
(require 'lim-module-completion)
(require 'lim-module-dired)
(require 'lim-module-org)
(require 'lim-module-window)
(require 'lim-module-ai)
(require 'lim-module-dev)
(require 'lim-module-keybind)
