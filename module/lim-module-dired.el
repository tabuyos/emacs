(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :hook
  (dired-sidebar-mode . lim-simple-whitespace-disable)
  (dired-sidebar-mode . auto-revert-mode)
  :bind
  ( :map dired-sidebar-mode-map
    ("<mouse-2>" . nil)
    ("<double-mouse-1>" . dired-sidebar-mouse-subtree-cycle-or-find-file))
  :config
  (advice-add 'dired-sidebar-mouse-subtree-cycle-or-find-file :after 'dired-sidebar-revert)
  (setq dired-listing-switches "-Ahlv"))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-dwim-target t))

(use-package dired-x :ensure nil)

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
  ;; (setq dired-make-directory-clickable t)
  (setq dired-free-space nil)
  (setq dired-omit-verbose nil)
  ;; (setq dired-mouse-drag-files t)

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (add-hook 'dired-mode-hook #'dired-omit-mode))

(use-package trashed
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(provide 'lim-module-dired)
