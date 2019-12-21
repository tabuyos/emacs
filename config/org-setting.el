(add-hook 'org-mode-hook 'toggle-truncate-lines)
(with-eval-after-load 'org
  (add-to-list 'org-export-backends 'md))
;; (require 'ox-md)
(provide 'org-setting)