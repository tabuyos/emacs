;; User Script.
;; You can edit the file to change some profile.
;; I hope that you can edit Emacs Lisp manually.
;; Plz enjoy it.

;; Setting my plugins source address.
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
							("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
							("Marmalade" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")
							("Org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
;; You might already have this line
(package-initialize)


(setq server-name "Tabuyos") ;; Setting Emacs server's name.
(server-start) ;; Startup server.

(display-time-mode t) ;; Display time in state bar.
(setq display-time-24hr-format t) ;; Use 24H format.
(setq display-time-day-and-date t) ;; Dispaly day and date.
(tool-bar-mode 0) ;; Disable tool bar.
(menu-bar-mode 0) ;; Disable menu bar.
(scroll-bar-mode 0) ;; Disable scroll bar.
(setq-default tab-width 4) ;; Setting tab width 4.
(setq lisp-indent-offset 4) ;; Setting Lisp mode indent width.
(setq visible-bell 0) ;; Disable beep of bell.
(delete-selection-mode t) ;; When selection can be delete.
(setq set-fill-column 100) ;; Setting max column.

(set-keyboard-coding-system 'utf-8) ;; Setting keyboard coding.
										; (set-selection-coding-system 'utf-8) ;; Setting keyboard coding.
(set-buffer-file-coding-system 'utf-8) ;; Setting buffer file coding.
										; (set-default buffer-file-coding-system 'utf-8) ;; Setting buffer file coding.
										; (set-default-coding-systems 'utf-8) ;; Setting buffer file coding.
(prefer-coding-system 'utf-8) ;; Setting default coding.
										; (set-clipboard-coding-system 'utf-8) ;; Setting clipboard coding.
(set-terminal-coding-system 'utf-8) ;; Setting terminal coding.
										; (setq-default pathname-coding-system 'utf-8) ;; Setting filepath coding.
										; (set-file-name-coding-system 'utf-8) ;; Setting filename coding.
(setq default-buffer-file-coding-system 'utf-8-unix) ;; Setting default buffer file coding.
(set-language-environment 'utf-8) ;; Setting environment coding.
										; (set-language-environment 'Chinese-GB) ;; Setting environment coding.

(show-paren-mode t) ;; Enable highlight paren.
(electric-pair-mode t) ;; Enable autopair paren.
(setq inhibit-splash-screen t) ;; Disable startup page.
;; (global-linum-mode t) ;; Enable line number mode. ;; Emacs lastest version NOT contain.
(global-display-line-numbers-mode t) ;; Enable line number mode.
(setq make-backup-files nil) ;; Disable backup file.
(set-default-font "Consolas 12") ;; English font.
(set-fontset-font "fontset-default" 'han '("楷体" . "unicode-bmp")) ;; Chinese font.
(set-fontset-font "fontset-default" 'symbol '("Cambria Math" . "unicode-bmp")) ;; Math symbol font.

(setq desktop-restore-eager 5) ;; Setting startup autoload max file.
(setq desktop-save-mode t) ;; Enable desktop save mode.

;; Autoload my settings.
(add-to-list 'load-path "~/.emacs.d/config") ;; Add autoload path to configure file.
(require 'dracula-setting) ;; Use dracula theme.
(require 'slime-setting) ;; Use slime for Lisp in Emacs.
(require 'auctex-setting) ;; Use AucTeX mode in Emacs.
(require 'org-setting) ;; Setting Org-mode in Emacs.
(require 'web-setting) ;; Setting Web-mode in Emacs.
(require 'my-setting) ;; Setting customize in Emacs.
;; (require 'minimap-setting) ;; Use Minimap in Emacs
(require 'lsp-java-setting) ;; Use lsp of java.
(require 'awesome-pair-setting) ;; Use autopair paren.
(require 'indent-setting) ;; Use indent hint line in Emacs.

