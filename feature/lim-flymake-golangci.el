;;; lim-flymake-golangci.el --- Lim's flymake backend for golangci-lint -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; Golangci-Lint for Lim

;;; Code:

(require 'project)

(defvar flymake-golangci-executable "golangci-lint" "Path to golangci-lint executable.")

(defvar flymake-golangci-args nil "Flags sent to golangci")

(defvar-local flymake-golangci--proc nil)

(defun flymake-golangci--match-regex (filename)
  (format "\\(%s\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\) \\(([A-Z0-9]+)\\)"
          filename))

(defun flymake-golangci (report-fn &rest _args)
  "Flymake backend function for golangci-lint, a linter for Go. "
  (unless (executable-find flymake-golangci-executable)
    (error "Cannot find golangci-lint, is it installed?"))
  (when (process-live-p flymake-golangci--proc)
    (kill-process flymake-golangci--proc))
  (let* ((source (current-buffer))
         (match-regexp (flymake-golangci--match-regex
                        (file-name-nondirectory (buffer-file-name source)))))
    (save-restriction
      (widen)
      ;; Reset the `flymake-golangci--proc' process to a new process
      (setq
       flymake-golangci--proc
       (make-process
        :name "flymake-golangci" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *flymake-golangci*")
        ;; Run golangci, no need to pass config file as golangci looks for it
        :command `(,flymake-golangci-executable "run" ,(file-name-directory
                                                        (buffer-file-name source)))
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might be simply suspended.
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                ;; Only proceed if `proc' is the same as `flymake-golangci--proc',
                ;; which indicates that `proc' is not an obsolete process.
                (if (with-current-buffer source (eq proc flymake-golangci--proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      ;; Parse the buffer, collect them and call `report-fn'.
                      (cl-loop
                       while (search-forward-regexp
                              match-regexp
                              nil t)
                       for msg = (format "golangci-lint %s: %s"
                                         (match-string 5)
                                         (match-string 4))
                       for (beg . end) = (flymake-diag-region
                                          source
                                          (string-to-number (match-string 2))
                                          (string-to-number (match-string 3)))
                       when (and beg end)
                       collect (flymake-make-diagnostic source
                                                        beg
                                                        end
                                                        :error
                                                        msg)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              ;; Cleanup the temporary buffer used to hold the check's output.
              (kill-buffer (process-buffer proc)))))))
      ;; Send the buffer contents to the process's stdin, followed by EOF.
      (process-send-region flymake-golangci--proc (point-min) (point-max))
      (process-send-eof flymake-golangci--proc))))

;;;###autoload
(defun flymake-golangci-load-backend ()
  "Loads golangci into `flymake-diagnostic-functions'."
  (add-hook 'flymake-diagnostic-functions 'flymake-golangci nil t))

;;;###autoload
(defun flymake-golangci-lint-project ()
  "Lint entire project with `golangci-lint'."
  (interactive)
  (let ((current-directory (project-current)))
    (call-process flymake-golangci-executable
                  nil
                  (get-buffer-create "*golangci-lint run*")
                  nil
                  "run")))

;;;###autoload
(defun flymake-golangci-clear-cache ()
  "Clear `golangci-lint' cache."
  (interactive)
  (unless (executable-find flymake-golangci-executable)
    (error "Cannot find golangci-lint, is it installed?"))
  (call-process flymake-golangci-executable nil nil nil "cache" "clean"))

(provide 'lim-flymake-golangci)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
