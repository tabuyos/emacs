(add-to-list 'load-path "~/.emacs.d/packages/slime")
(require 'slime-autoloads)
;; (require 'slime)
;; (slime-setup)
(setq inferior-lisp-program "E:/opt/SteelBankCommonLisp/1.4.14/sbcl.exe")
(setq slime-contribs '(slime-fancy))
(provide 'slime-setting)