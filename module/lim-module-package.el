(use-package lim-package
  :ensure nil
  :config
  (setq package-user-dir archives-dir
        package-archives archives-source
        package-archive-priorities archives-priorities)
  (setq package-install-upgrade-built-in nil)
  (unless (bound-and-true-p package--initialized)
    (setq package-enable-at-startup nil)
    (package-initialize))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents :async)
    (package-install 'use-package))
  (eval-and-compile
    (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file)))
    (setq use-package-always-ensure t)
    (setq use-package-expand-minimally t)
    (setq use-package-compute-statistics t)
    (setq use-package-enable-imenu-support t))

  (integration-diminish)
  ;; (integration-straight)
  ;; (integration-quelpa)
  ;; (integration-elpaca)
  )

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(use-package auto-package-update
  :unless (daemonp)
  :custom
  ;; in days
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-at-time "03:00"))

(use-package s)

(use-package f)

(use-package async
  :config
  (async-bytecomp-package-mode 1))

(provide 'lim-module-package)
