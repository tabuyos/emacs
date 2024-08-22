(use-package nerd-icons
  :diminish nil)

(use-package nerd-icons-completion
  :diminish nil
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (with-eval-after-load 'marginalia
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))

(use-package nerd-icons-corfu
  :after corfu
  :diminish nil
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :diminish nil
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'lim-module-icon)
