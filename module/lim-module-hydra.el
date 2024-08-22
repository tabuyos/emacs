(use-package hydra
  :config
  (setq lv-use-separator t)
  (setq hydra-is-helpful nil))

(use-package use-package-hydra
  :after hydra)

(use-package posframe)

(use-package hydra-posframe
  :vc ( :url "https://github.com/Ladicle/hydra-posframe.git"
        :rev :newest)
  :hook (after-init . hydra-posframe-mode))

(use-package major-mode-hydra)

(provide 'lim-module-hydra)
