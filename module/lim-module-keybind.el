(use-package lim-keymap :ensure nil)

(use-package lim-hydra
  :ensure nil
  :bind
  (("," . lim-hydra/body)
   :map isearch-mode-map
   ("," . lim-hydra/body)))

(use-package lim-minibuffer
  :ensure nil
  :bind
  (:map minibuffer-mode-map
   ("," . lim-minibuffer-mode-enable))
  :hook
  (minibuffer-exit . lim-minibuffer-mode-disable))

(provide 'lim-module-keybind)
