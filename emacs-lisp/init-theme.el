;;; package --- tabuyos-init --- init-theme.el
;;; Commentary:
;; This is initial configuration for theme.
;;; Code:

(eval-when-compile
  (require 'init-constant))

;; DoomThemes-
(use-package doom-themes
  :custom-face
  (cursor ((t (:background "BlanchedAlmond"))))
  :config
  ;; flashing mode-line on errors.
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-one t))
;; -DoomThemes

;; DoomModeLine-
(use-package doom-modeline
  :custom
  ;; Dom't compact font caches during GC.  Windows Laggy Issue
  (inhibit-compacting-cont-caches t)
  (doom-modeline-minor-modes t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-height 15)
  :config
  (doom-modeline-mode))
;; -DoomModeLine

(provide 'init-theme)
;;; init-theme.el ends here.
