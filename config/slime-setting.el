(add-to-list 'load-path "~/.emacs.d/packages/slime")
(require 'slime-autoloads)
;; (require 'slime)
(slime-setup '(slime-scratch slime-editing-commands))
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))
(provide 'slime-setting)
