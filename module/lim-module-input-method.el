;; (use-package rime
;;   :config
;;   (setq default-input-method "rime")
;;   (setq rime-show-candidate 'minibuffer)
;;   (setq rime-user-data-dir (expand-file-name "librime/rime-ice" user-emacs-directory))
;;   (setq rime-emacs-module-header-root "/opt/homebrew/opt/emacs-plus@30/include")
;;   (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
;;   :bind
;;   ( :map rime-mode-map
;;     ("C-," . 'rime-send-keybinding)))

(provide 'lim-module-input-method)
