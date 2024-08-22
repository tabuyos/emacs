(use-package lim-dev :ensure nil)

(use-package imenu-list
  :config
  (setq-default imenu-list-mode-line-format
        '("%e"
          (:propertize "%b" face mode-line-buffer-id) " - "
          (:eval (buffer-name imenu-list--displayed-buffer)) " "
          )))

(use-package magit
  :defer t
  :commands magit-status
  :config
  (setq magit-diff-refine-hunk 'all)
  (setq magit-display-buffer-function
        'magit-display-buffer-fullframe-status-v1)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-log-arguments
        '("--graph" "--color" "--decorate" "--show-signature" "--follow" "-n256"))
  (setq magit-log-margin-show-committer-date t)
  (setq magit-log-remove-graph-args
        '("--follow" "--grep" "-G" "-S" "-L"))
  (setq magit-todos-insert-after'(bottom))
  )

(use-package magit-todos :after magit)

(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode)
  :config
  (setq magit-delta-default-dark-theme "ef-elea-dark"))

(use-package forge
  :after magit
  :demand t
  :custom-face
  (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
  :init (setq forge-topic-list-columns
              '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
                ("Title" 60 t nil title  nil)
                ("State" 6 t nil state nil)
                ("Updated" 10 t nil updated nil))))

(use-package vc-hooks
  :ensure nil
  :config
  (setq vc-follow-symlinks t)
  (setq vc-allow-async-revert t)
  ;; (setq vc-handled-backends '(Git))
  )

(use-package diff-hl
  :hook ((after-init         . global-diff-hl-mode)
         (dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; When Emacs runs in terminal, show the indicators in margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

(use-package ediff
  :ensure nil
  :hook ((ediff-before-setup . ediff-save-window-conf)
         (ediff-quit         . ediff-restore-window-conf))
  :config
  (defvar local-ediff-saved-window-conf nil)

  (defun ediff-save-window-conf ()
    (setq local-ediff-saved-window-conf (current-window-configuration)))

  (defun ediff-restore-window-conf ()
    (when (window-configuration-p local-ediff-saved-window-conf)
      (set-window-configuration local-ediff-saved-window-conf)))
  :custom
  (ediff-highlight-all-diffs t)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

(use-package git-gutter
  :defer t
  :config
  (global-git-gutter-mode 1))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package blamer
  :defer 20
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background unspecified
                   :height 140
                   :italic t)))
  :config
  (setq blamer-idle-time 0.3)
  (setq blamer-min-offset 80)

  (global-blamer-mode 1))

(use-package git-modes
  :mode ("/.dockerignore\\'" . gitignore-mode))

(use-package browse-at-remote
  :bind (:map vc-prefix-map
              ("B" . browse-at-remote)))

(use-package ws-butler
  :diminish
  :config
  (ws-butler-global-mode 1))

(use-package whitespace-cleanup-mode
  :diminish
  :commands whitespace-cleanup-mode)

(use-package editorconfig
  :diminish
  :config
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  (editorconfig-mode 1))

(use-package goto-addr
  :ensure nil
  :hook
  ((text-mode . goto-address-mode)
   (prog-mode . goto-address-prog-mode))
  :bind
  (("<mouse-2>" . ignore)
   :map goto-address-highlight-keymap
   ("<mouse-2>" . ignore))
  :config
  (setq goto-address-url-mouse-face nil)
  (setq goto-address-mail-mouse-face nil))

(use-package lim-project
  :ensure nil
  :autoload (lim-project-root)
  :hook (after-init . global-lim-project-mode))

(use-package lim-treesit :ensure nil)

;; (use-package tabspaces
;;   :hook (after-init . tabspaces-mode)
;;   :config
;;   (setq tab-bar-show nil)

;;   (setq tabspaces-use-filtered-buffers-as-default t)
;;   (setq tabspaces-default-tab "Default")
;;   (setq tabspaces-remove-to-default t)
;;   (setq tabspaces-include-buffers '("*scratch*" "*Messages*"))
;;   ;; sessions
;;   (setq tabspaces-session t)
;;   (setq tabspaces-session-auto-restore t)

;;   ;; Filter Buffers for Consult-Buffer
;;   (with-eval-after-load 'consult
;;     ;; hide full buffer list (still available with "b" prefix)
;;     (consult-customize consult--source-buffer :hidden t :default nil)
;;     ;; set consult-workspace buffer list
;;     (defvar consult--source-workspace
;;       (list :name     "Workspace Buffer"
;;             :narrow   ?w
;;             :history  'buffer-name-history
;;             :category 'buffer
;;             :state    #'consult--buffer-state
;;             :default  t
;;             :items    (lambda () (consult--buffer-query
;;                                   :predicate #'tabspaces--local-buffer-p
;;                                   :sort 'visibility
;;                                   :as #'buffer-name)))
;;       "Set workspace buffer list for consult-buffer.")
;;     (add-to-list 'consult-buffer-sources 'consult--source-workspace))
;;   )

(use-package eglot
  :ensure nil
  :config
  (setq eglot-autoshutdown t))

(use-package eglot-booster
  :vc ( :url "https://github.com/jdtsmith/eglot-booster.git"
        :rev :newest)
	:after eglot
	:config	(eglot-booster-mode))

(use-package consult-eglot)

(use-package consult-eglot-embark
  :config
  (with-eval-after-load 'embark
    (consult-eglot-embark-mode)))

(use-package flycheck)

(use-package lsp-mode
  :commands (lsp lsp-format-buffer lsp-organize-imports)
  :diminish
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-log-io t)
  ;; (setq lsp-keep-workspace-alive nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; (setq lsp-progress-spinner-type 'progress-bar-filled)
  ;; (setq lsp-enable-file-watchers nil)
  ;; (setq lsp-enable-folding nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-enable-text-document-color nil)
  ;; (setq lsp-enable-indentation nil)
  ;; (setq lsp-enable-on-type-formatting nil)
  :hook
  ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred)
  :config
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      ;; for check lsp-server-present?
      (if (and (not test?)
               ;; see lsp-resolve-final-command, it would add extra shell wrapper
               (not (file-remote-p default-directory))
               lsp-use-plists
               ;; native json-rpc
               (not (functionp 'json-rpc-connection))
               (executable-find "emacs-lsp-booster"))
          (progn
            ;; resolve command from exec-path (in case not found in $PATH)
            (when-let ((command-from-exec-path (executable-find (car orig-result))))
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(use-package lsp-mode
  :custom
  ;; we use Corfu!
  (lsp-completion-provider :none)
  :init
  (defun lim/lsp-mode-setup-completion ()
    "Configure corfu for lsp-mode"
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless flex basic))
    (add-hook 'orderless-style-dispatchers #'lim-orderless-dispatch-flex-first nil t)
    ;; Optionally configure the cape-capf-buster.
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))
  
  :hook
  (lsp-completion-mode . lim/lsp-mode-setup-completion))

(use-package lsp-ui :commands lsp-ui-mode)

;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))

(use-package consult-lsp
  :bind
  ( :map lsp-mode-map
    ("C-M-." . consult-lsp-symbols)))

(use-package lsp-bridge
  :vc ( :url "https://github.com/manateelazycat/lsp-bridge.git"
        :rev :newest)
  :commands (lsp-bridge-mode lsp-bridge-code-format)
  :diminish
  :defer t
  :bind
  ( :map lsp-bridge-mode-map
    ([remap xref-find-definitions] . lsp-bridge-find-def)
    ([remap xref-find-references] . lsp-bridge-find-references)
    ([remap xref-go-back] . lsp-bridge-find-def-return))
  :init
  (setq lsp-bridge-org-babel-lang-list nil)
  :config
  (setq lsp-bridge-enable-log t)
  (setq lsp-bridge-user-langserver-dir (f-join (lim-project-root) lim-project-identifier "lspserver"))
  (setq lsp-bridge-user-multiserver-dir (f-join (lim-project-root) lim-project-identifier "multilspserver"))
  (setq lsp-bridge-enable-hover-diagnostic t))

(use-package lim-lsp :ensure nil)

(use-package sh-script
  :ensure nil
  :hook
  (sh-mode . bash-ts-mode)
  :init
  (lim-treesit-enable 'bash))

(use-package c-ts-mode
  :ensure nil
  :init
  (lim-treesit-enable 'c))

(use-package cc-mode
  :ensure nil
  :hook (c-mode . c-ts-mode))

(use-package c-ts-mode
  :ensure nil
  :init
  (lim-treesit-enable 'cpp)
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

(use-package cc-mode
  :ensure nil
  :hook
  (c++-mode . c-ts-mode)
  (c-or-c++-mode . c-or-c++-ts-mode))

(use-package css-mode
  :ensure nil
  :hook (css-mode . css-ts-mode)
  :init
  (lim-treesit-enable 'css))

(use-package cmake-ts-mode
  :ensure nil
  :init
  (lim-treesit-enable 'cmake))

(use-package cmake-mode
  :hook (cmake-mode . cmake-ts-mode))

(use-package csharp-mode
  :ensure nil
  :hook (csharp-mode . csharp-ts-mode)
  :init
  (lim-treesit-enable 'c-sharp))

(use-package dockerfile-ts-mode
  :init
  (lim-treesit-enable 'dockerfile))

(use-package dockerfile-mode
  :hook
  (dockerfile-mode . dockerfile-ts-mode))

(use-package elisp-mode
  :ensure nil
  :functions (lim/elisp-ts-support)
  :hook (emacs-lisp-mode . lim/elisp-ts-support)
  :init
  (lim-treesit-enable 'elisp)

  (defun lim/elisp-ts-support ()
    (when (treesit-language-available-p 'elisp)
      (treesit-parser-create 'elisp)
      (treesit-major-mode-setup))))

(use-package go-ts-mode
  :ensure nil
  :functions (lim/whitespace-no-tab lim/install-go-tools)
  ;; :mode
  ;; ("\\.go\\'" . go-ts-mode)
  :hook
  (go-ts-mode . lim-lsp-ensure-for-eglot)
  (go-ts-mode . lim/whitespace-no-tab)
  (go-ts-mode . lim/install-go-tools)
  :init
  (lim-treesit-enable 'go)
  :config
  (setq go-ts-mode-indent-offset 2)

  (defun lim/whitespace-no-tab ()
    "Buffer local `whitespace-style'."
    (make-local-variable 'whitespace-style)
    (setq whitespace-style
          '(face trailing lines-tail missing-newline-at-eof)))

  (defun lim/install-go-tools ()
    "Install some go tools."
    (call-interactively 'lim-dev-install-go-tools))

  (cl-defmethod eglot-initialization-options (server &context (major-mode go-ts-mode))
    (lim-lsp-load-lsp-options "gopls"))
  )

;; (use-package flycheck-golangci-lint
;;   :hook (go-ts-mode . flycheck-golangci-lint-setup))

;; (use-package flymake-golangci
;;   :hook (go-ts-mode . flymake-golangci-load))

;; (use-package lim-flymake-golangci
;;   :ensure nil
;;   :functions (lim/golangci-config)
;;   :hook
;;   (eglot-managed-mode . lim/golangci-config)
;;   ;; (go-mode . flymake-golangci-load-backend)
;;   (go-ts-mode . flymake-golangci-load-backend)
;;   :config
;;   (defun lim/golangci-config ()
;;     (when (derived-mode-p '(go-mode go-ts-mode))
;;       (flymake-golangci-load-backend))))

(use-package go-mode
  :hook
  (go-mode . go-ts-mode)
  :config
  (setq gofmt-command "goimports")
  (add-to-list 'lim-dev-go-tools-alist
               '(gopls . "golang.org/x/tools/gopls"))
  (add-to-list 'lim-dev-go-tools-alist
               '(godef . "github.com/rogpeppe/godef"))
  (add-to-list 'lim-dev-go-tools-alist
               '(goimports . "golang.org/x/tools/cmd/goimports"))
  (add-to-list 'lim-dev-go-tools-alist
               '(godoc . "golang.org/x/tools/cmd/godoc"))
  (add-to-list 'lim-dev-go-tools-alist
               '(golangci-lint-langserver . "github.com/nametake/golangci-lint-langserver"))
  (add-to-list 'lim-dev-go-tools-alist
               '(golangci-lint . "github.com/golangci/golangci-lint/cmd/golangci-lint")))

(use-package godoctor
  :config
  (add-to-list 'lim-dev-go-tools-alist
               '(godoctor . "github.com/godoctor/godoctor")))

(use-package go-tag
  :config
  (add-to-list 'lim-dev-go-tools-alist
               '(gomodifytags . "github.com/fatih/gomodifytags")))

(use-package go-playground)

(use-package go-fill-struct
  :config
  (add-to-list 'lim-dev-go-tools-alist
               '(fillstruct . "github.com/davidrjenni/reftools/cmd/fillstruct")))

(use-package gotest)

(use-package go-gen-test
  :config
  (add-to-list 'lim-dev-go-tools-alist
               '(gotests . "github.com/cweill/gotests/...")))

(use-package go-impl
  :config
  (add-to-list 'lim-dev-go-tools-alist
               '(impl . "github.com/josharian/impl"))
  (add-to-list 'lim-dev-go-tools-alist
               '(godoc . "golang.org/x/tools/cmd/godoc")))

(use-package go-dlv
  :config
  (add-to-list 'lim-dev-go-tools-alist
               '(dlv . "github.com/go-delve/delve/cmd/dlv")))

(use-package go-ts-mode
  :ensure nil
  ;; :mode
  ;; ("go\\.mod\\'" . go-mod-ts-mode)
  :hook
  (go-mod-ts-mode . lim/whitespace-no-tab)
  :init
  (lim-treesit-enable 'gomod))

(use-package go-mode
  :hook
  (go-dot-mod-mode . go-mod-ts-mode))

(use-package haskell-mode
  :functions (lim/haskell-ts-support)
  :hook
  (haskell-mode . lim/haskell-ts-support)
  (haskell-mode . lim-lsp-ensure-for-eglot)
  :init
  (lim-treesit-enable 'haskell)

  (defun lim/haskell-ts-support ()
    (treesit-parser-create 'haskell)
    (treesit-major-mode-setup)))

(use-package html-ts-mode
  :ensure nil
  :init (lim-treesit-enable 'html))

;; (use-package sgml-mode
;;   :ensure nil
;;   :hook ((html-mode mhtml-mode sgml-mode) . html-ts-mode))

(use-package java-ts-mode
  :ensure nil
  :hook
  (java-ts-mode . lim-lsp-ensure-for-lsp-bridge)
  :init (lim-treesit-enable 'java)
  :config
  (setq java-ts-mode-indent-offset 4)

  ;; (cl-defmethod eglot-initialization-options (server &context (major-mode java-ts-mode))
  ;;   (lim-lsp-load-lsp-options "jdtls"))
  )

(use-package cc-mode
  :ensure nil
  :hook (java-mode . java-ts-mode))

(use-package js
  :ensure nil
  :hook
  (js-ts-mode . lim-lsp-ensure-for-eglot)
  (js-mode . js-ts-mode)
  (javascript-mode . js-ts-mode)
  :init (lim-treesit-enable 'javascript))

(use-package json-ts-mode
  :ensure nil
  :hook
  (json-ts-mode . lim-lsp-ensure-for-eglot)
  :init (lim-treesit-enable 'json)
  :config
  (setq json-ts-mode-indent-offset 2))

(use-package js
  :ensure nil
  :hook (js-json-mode . json-ts-mode))

(use-package json-mode
  :hook ((json-mode jsonc-mode) . json-ts-mode))

(use-package json-navigator)

(use-package kotlin-ts-mode
  :mode "\\.kts?\\'"
  :hook
  (kotlin-ts-mode . lim-lsp-ensure-for-eglot)
  :init (lim-treesit-enable 'kotlin))

;; (use-package kotlin-mode
;;   :hook (kotlin-mode . kotlin-ts-mode))

(use-package lua-ts-mode
  :ensure nil
  :hook
  (lua-ts-mode . lim-lsp-ensure-for-eglot)
  :init (lim-treesit-enable 'lua))

(use-package make-mode
  :ensure nil
  :functions (lim/make-ts-support)
  :hook (makefile-mode . lim/make-ts-support)
  :init
  (lim-treesit-enable 'make)

  (defun lim/make-ts-support ()
    (treesit-parser-create 'make)
    (treesit-major-mode-setup)))

(use-package markdown-ts-mode
  :commands markdown-ts-mode
  :hook
  (markdown-ts-mode . lim-lsp-ensure-for-eglot)
  :init
  (lim-treesit-enable 'markdown)
  (lim-treesit-enable 'markdown-inline)
  (add-to-list 'eglot-server-programs '(markdown-ts-mode . ("marksman" "server"))))

(use-package markdown-mode
  :functions (lim/markdown-ts-support)
  :hook (markdown-mode . lim/markdown-ts-support)
  :init
  (lim-treesit-enable 'markdown)
  (lim-treesit-enable 'markdown-inline)

  (defun lim/markdown-ts-support ()
    (treesit-parser-create 'markdown)
    (treesit-major-mode-setup)))

(use-package ocaml-ts-mode
  :hook
  (ocaml-ts-mode . lim-lsp-ensure-for-eglot)
  :init
  (lim-treesit-enable 'ocaml))

(use-package tuareg
  :hook (tuareg-mode . ocaml-ts-mode))

(use-package caml
  :hook (caml-mode . ocaml-ts-mode))



(use-package python
  :ensure nil
  :hook
  (python-ts-mode . lim-lsp-ensure-for-eglot)
  (python-mode . python-ts-mode)
  :init (lim-treesit-enable 'python))

(use-package php-ts-mode
  :vc ( :url "https://github.com/emacs-php/php-ts-mode.git"
        :rev :newest)
  :hook
  (php-ts-mode . lim-lsp-ensure-for-eglot)
  :init (lim-treesit-enable 'php))

(use-package typescript-ts-mode
  :ensure nil
  :hook
  (typescript-ts-mode . lim-lsp-ensure-for-eglot)
  :init (lim-treesit-enable 'typescript))

(use-package typescript-mode
  :hook (typescript-mode . typescript-ts-mode))

(use-package tsx-ts-mode
  :ensure nil
  :hook
  (tsx-ts-mode . lim-lsp-ensure-for-eglot)
  :init (lim-treesit-enable 'tsx))

;; (use-package tsx-mode
;;   :vc ( :url "https://github.com/orzechowskid/tsx-mode.el.git"
;;         :rev :newest)
;;   :hook (tsx-mode . tsx-ts-mode))

(use-package ruby-ts-mode
  :ensure nil
  :hook
  (ruby-ts-mode . lim-lsp-ensure-for-eglot)
  :init (lim-treesit-enable 'ruby))

(use-package ruby-mode
  :ensure nil
  :hook (ruby-mode . ruby-ts-mode))

(use-package inf-ruby
  :hook
  (ruby-ts-mode-hook . inf-ruby-minor-mode)
  ;; Auto breakpoint
  (compilation-filter . inf-ruby-auto-enter)
  :init
  (setq inf-ruby-default-implementation "pry"))

(use-package rust-ts-mode
  :ensure nil
  :hook
  (rust-ts-mode . lim-lsp-ensure-for-eglot)
  :init (lim-treesit-enable 'rust)
  :config
  (setq rust-ts-mode-indent-offset 4)

  (cl-defmethod eglot-initialization-options (server &context (major-mode rust-ts-mode))
    (lim-lsp-load-lsp-options "rust-analyzer")))

(use-package rust-mode
  :defer t
  :hook (rust-mode . rust-ts-mode)
  ;; :config
  ;; (setq rust-format-on-save t)
  )

(use-package rustic
  :defer t
  :hook (rustic-mode . rust-ts-mode)
  ;; :config
  ;; (setq rustic-lsp-client 'eglot)
  ;; (setq rustic-format-on-save t)
  )

(use-package sql
  :ensure nil
  :functions (lim/sql-ts-support)
  :hook (sql-mode . lim/sql-ts-support)
  :init
  (setq sql-mysql-options '("--protocol=tcp" "--prompt=" "--disable-pager"))
  
  (defun lim/sql-ts-support ()
    (treesit-parser-create 'sql)
    (treesit-major-mode-setup)))

;; (use-package vue-ts-mode
;;   :vc ( :url "https://github.com/8uff3r/vue-ts-mode.git"
;;         :rev :newest)
;;   :hook
;;   (vue-ts-mode . lim-lsp-ensure-for-eglot)
;;   :init (lim-treesit-enable vue))

(use-package vue-mode
  :hook
  (vue-ts-mode . lim-lsp-ensure-for-lsp-bridge)
  :init (lim-treesit-enable 'vue))

(use-package yaml-ts-mode
  :ensure nil
  :hook
  (yaml-ts-mode . lim-lsp-ensure-for-eglot)
  :init (lim-treesit-enable 'yaml))

(use-package yaml-mode
  :hook (yaml-mode . yaml-ts-mode))





(use-package emmet-mode
  :hook
  (sgml-mode . emmet-mode)
  (css-mode . emmet-mode))

(use-package web-mode
  :after (eglot emmet-mode)
  :hook
  (web-mode . lim-lsp-ensure-for-eglot)
  (web-mode . emmet-mode))

(use-package treesit
  :ensure nil
  :config
  (setq treesit-font-lock-level 4)

  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (message "Install %s language grammer..." lang)
      (treesit-install-language-grammar lang))))

(provide 'lim-module-dev)
