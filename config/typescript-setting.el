(use-package typescript-mode)
(use-package tide
	:ensure t
	:after (typescript-mode company flycheck)
	:hook ((typescript-mode . tide-setup)
			  (typesctipt-mode . yas-minor-mode-on)
			  (typescript-mode . tide-hl-identifier-mode)
			  (before-save . tide-format-before-save)))

(provide 'typescript-setting)
