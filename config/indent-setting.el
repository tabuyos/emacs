(add-to-list 'load-path "~/.emacs.d/packages/highlight-indent-guides")
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?\|)
(provide 'indent-setting)
