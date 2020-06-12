(add-to-list 'load-path "~/.emacs.d/packages/web-mode")
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; settings
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  ;; indent setting
  (setq web-mode-markup-indent-offset 4)
  ;; comment style setting
  (setq web-mode-comment-style 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)


;; keybinding
(defun web-mode-keybinding-settings ()
  "Settings for keybinding."
  (eal-define-keys
   '(web-mode-map)
   '(("C-c C-v" 'browse-url-of-file))))

(eval-after-load "web-mode"
  '(progn
     (define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-file)))
  ;;'web-mode-keybinding-settings)

(provide 'web-setting)
