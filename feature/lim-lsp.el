;;; lim-lsp.el --- Enhanced lsp -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Enhanced lsp.

;;; Code:

(require 'f)
(require 's)
(require 'jsonrpc)
(require 'lim-helper)
(require 'lim-project)

(require 'eglot)
(require 'lsp-mode)
(require 'lsp-bridge)

;;;###autoload
(defun lim-lsp-load-lsp-options (&optional lsp-server user-fn)
  "Load LSP's options in initilize."
  (let* ((name (or lsp-server (lim-helper-mode-id)))
         (filename (format "%s.json" name))
         (local-dir (lim-project-root))
         (default-dir user-emacs-directory)
         (local-file (f-join local-dir lim-project-identifier "lspserver" filename))
         (default-file (f-join default-dir "lspserver" filename)))
    (lim-helper-plist-merge
     (lim-helper-load-options local-file)
     (lim-helper-load-options default-file)
     (and user-fn (funcall user-fn)))))



;;; Java Enhanced

(defvar lim-lsp-java-jdt-uri-handling-patch-applied nil "Whether or not JDT uri handling is already patched.")

(defun lim-lsp-java-jdt-uri-handler (_op &rest args)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (let* ((uri (car args))
         (cache-dir (f-join (lim-project-root) ".cache" "jdtls"))
         (source-file
          (expand-file-name
           (f-join
            cache-dir
            (save-match-data
              (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot-current-server) :java/classFileContents (list :uri uri)))
            (metadata-file (format "%s.%s.metadata"
                                   (file-name-directory source-file)
                                   (file-name-base source-file))))
        (unless (file-directory-p cache-dir) (make-directory cache-dir t))
        (with-temp-file source-file (insert content))
        (with-temp-file metadata-file (insert uri))))
    source-file))

(defun lim-lsp-java-wrap-legacy-eglot--path-to-uri (original-fn &rest args)
  "Hack until eglot is updated.
ARGS is a list with one element, a file path or potentially a URI.
If path is a jar URI, don't parse. If it is not a jar call ORIGINAL-FN."
  (let ((path (file-truename (car args))))
    (if (equal "jdt" (url-type (url-generic-parse-url path)))
        path
      (apply original-fn args))))

(defun lim-lsp-java-wrap-legacy-eglot--uri-to-path (original-fn &rest args)
  "Hack until eglot is updated.
ARGS is a list with one element, a URI.
If URI is a jar URI, don't parse and let the `lim-lsp-java-jdt-uri-handler'
handle it. If it is not a jar call ORIGINAL-FN."
  (let ((uri (car args)))
    (if (and (stringp uri)
             (string= "jdt" (url-type (url-generic-parse-url uri))))
        uri
      (apply original-fn args))))

(defun lim-lsp-java-jdthandler-patch-eglot ()
  "Patch old versions of Eglot to work with Jdthandler."
  (interactive)
  ;; TODO Remove when eglot is updated in melpa
  ;; See also https://debbugs.gnu.org/cgi/bugreport.cgi?bug=58790
  ;; See also https://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob;f=lisp/progmodes/eglot.el#l1558
  (unless (or (and (advice-member-p #'lim-lsp-java-wrap-legacy-eglot--path-to-uri 'eglot--path-to-uri)
                   (advice-member-p #'lim-lsp-java-wrap-legacy-eglot--uri-to-path 'eglot--uri-to-path))
              (<= 29 emacs-major-version))
    (advice-add 'eglot--path-to-uri :around #'lim-lsp-java-wrap-legacy-eglot--path-to-uri)
    (advice-add 'eglot--uri-to-path :around #'lim-lsp-java-wrap-legacy-eglot--uri-to-path)
    (message "[lim-lsp-java-jdthandler-patch-eglot] Eglot successfully patched.")))

;;;###autoload
(defun lim-lsp-java-initialize ()
  "Initialized Java customize config."
  (unless lim-lsp-java-jdt-uri-handling-patch-applied
    (add-to-list 'file-name-handler-alist '("\\`jdt://" . lim-lsp-java-jdt-uri-handler))
    (lim-lsp-java-jdthandler-patch-eglot)
    (setq lim-lsp-java-jdt-uri-handling-patch-applied t)
    ))



(defun lim-lsp-organize-imports-for-eglot ()
  "Call `eglot-code-action-organize-imports' by interactively."
  (call-interactively 'eglot-code-action-organize-imports))

(defun lim-lsp-install-save-hooks-for-eglot ()
  "Install save hooks for lsp."
  (add-hook 'before-save-hook #'eglot-format-buffer t t)
  ;; (add-hook 'before-save-hook #'lim-lsp-organize-imports-for-eglot t t)
  )

(defun lim-lsp-ensure-for-eglot ()
  "Start eglot."
  (eglot-ensure)
  (lim-lsp-install-save-hooks-for-eglot))



(defun lim-lsp-install-save-hooks-for-lsp-mode ()
  "Install save hooks for lsp."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(defun lim-lsp-ensure-for-lsp-mode ()
  "Start lsp-mode."
  (lsp-mode 1)
  (lim-lsp-install-save-hooks-for-lsp-mode))



(defun lim-lsp-code-format-for-lsp-bridge ()
  "Call `lsp-bridge-code-format by interactively."
  (call-interactively 'lsp-bridge-code-format))

(defun lim-lsp-install-save-hooks-for-lsp-bridge ()
  "Install save hooks for lsp."
  (add-hook 'before-save-hook #'lim-lsp-code-format-for-lsp-bridge t t))

(defun lim-lsp-ensure-for-lsp-bridge ()
  "Start lsp-bridge."
  (lsp-bridge-mode 1)
  (lim-lsp-install-save-hooks-for-lsp-bridge))

(provide 'lim-lsp)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
