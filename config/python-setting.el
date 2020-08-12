(use-package elpy
	:ensure t
	:init
	(elpy-enable)
	:hook
	((elpy-mode . py-autopep8-enable-on-save)))

(use-package py-autopep8
	:ensure t
	:after elpy)

(setenv "WORKON_HOME" "F:/Projects/EmacsWorkspace/Python")

(pyvenv-activate "F:/Projects/emacs_env")

(provide 'python-setting)
