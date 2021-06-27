;;; package --- tabuyos-init --- init-clojure.el
;;; Commentary:
;; This configuration for clojure
;;; Code:

;; ClojureMode-
(use-package clojure-mode)

(use-package elein)

(use-package cljsbuild-mode)

(use-package flycheck-clojure
  :defer t
  :config
  (flycheck-clojure-setup))

(use-package cider)
;; -ClojureMode

(provide 'init-clojure)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-clojure.el ends here
