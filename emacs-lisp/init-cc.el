;;; package --- tabuyos-init --- init-cc.el
;;; Commentary:
;; This configuration for cc mode(C/C++)
;;; Code:

(eval-when-compile
  (require 'init-constant))

;; CCLS-
(use-package ccls
  :defer t
  :if (not *sys/windows*)
  :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp)))
  :custom
  (ccls-executable (executable-find "ccls"))
  (ccls-sem-highlight-method 'font-lock)
  (ccls-enable-skipped-ranges nil)
  :config
  (lsp-register-clent
   (make-lsp-client
    :new-connection (lsp-tramp-connection (cons ccls-executable ccls-args))
    :major-modes '(c-mode c++-mode cuda-mode objc-mode)
    :server-id 'ccls-remote
    :multi-root nil
    :remote? t
    :notification-handlers
    (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
            ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
    :initialization-options (lambda () ccls-initialization-options)
    :library-folders-fn nil)))
;; -CCLS

;; CPPFontLock-
(use-package modern-cpp-font-lock
  :diminish t
  :init (modern-c++-font-lock-global-mode t))
;; -CPPFontLock

;; SmartCompile-
(use-package smart-compile)
;; -SmartCompile

;; MyQuickRun-
(defvar *c++-eshell-buffer* nil "C++ running buffer eshell.")

(defvar *c++-eshell-name* "c++-shell" "C++ running buffer name in eshell.")

(defconst gcc-command-list '(("c" . "gcc") ("c++" . "g++") ("cpp" . "g++")) "GNU Compiler Collection.")
(cond ((= 1 2) (print 1))
       ((= 2 3) (print 2))
       (t (print 3)))

(defun quick-run-c++ ()
  "Quick run C++."
  (interactive)
  (let* ((eshell-name *c++-eshell-name*)
	(command-string (compile-c++-by-gcc))
	(file (get-current-file-absolute-path))
	(dir (get-file-directory file))
	(basename (get-file-basename file))
	(extension ".exe"))
    (unless (buffer-live-p *c++-eshell-buffer*)
      (setq *c++-eshell-buffer* (gen-slient-eshell eshell-name)))

    (cond (*sys/windows* (setq extension ".exe"))
	  (*sys/linux* (setq extension ".out")))

    (with-current-buffer eshell-name
      (eshell-command (format "cd %s" (get-file-directory file)))
      (eshell-command command-string)
      (insert (format "%s" (get-pending-command-string (concat dir basename extension) default-directory)))
      (switch-to-buffer-other-window eshell-name)
      (eshell-send-input)
      )))

(defun compile-c++-by-gcc ()
  "Compile C++ by GNU Compiler Collection."
  (let* ((file (get-current-file-absolute-path))
	 (extension (get-file-extension file))
	 (gcc (assoc-default extension gcc-command-list))
	 (filename (get-file-name file))
	 (basename (get-file-basename file)))
    (format "%s %s -o %s" gcc filename basename)))

(defun gen-slient-eshell (name)
  "Generate slient eshell by NAME."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (eshell-mode))
    buf))

(defun get-current-file-absolute-path ()
  "Get current file absolute directory."
  (buffer-file-name))

(defun get-file-basename (filename)
  "Get current file basename with FILENAME."
  (file-name-base filename))

(defun get-file-directory (filename)
  "Get current file directory with FILENAME."
  (file-name-directory filename))

(defun get-file-name (filename)
  "Get current file directory with FILENAME."
  (file-name-nondirectory filename))

(defun get-file-extension (filename)
  "Get current file extension with FILENAME."
  (file-name-extension filename))

(defun get-pending-command-string (filename &optional directory)
  "Get pending command string with FILENAME and DIRECTORY."
  (file-relative-name filename directory))

(defun custom-c++-mode-hook ()
  "Custom C++ mode hook."
  (define-key c++-mode-map (kbd "C-c C-r") #'quick-run-c++))

(add-hook 'c++-mode-hook #'custom-c++-mode-hook)
;; -MyQuickRun

;; Go-
(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save))
;; -Go

(provide 'init-cc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-cc.el ends here
