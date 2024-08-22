(use-package calendar
  :ensure nil
  :commands (calendar)
  :config
  ;; Show calendar for diary entry
  (setq calendar-mark-diary-entries-flag t)
  ;; Show calendar for holiday
  (setq calendar-mark-holidays-flag t)
  (setq calendar-mode-line-format nil)
  (setq calendar-time-display-form
        '( 24-hours ":" minutes
           (when time-zone (format "(%s)" time-zone))))
  ;; Monday is the first day of the week
  (setq calendar-week-start-day 1)
  ;; Like year/month/day style
  (setq calendar-date-style 'iso)
  ;; Like +0800 style
  (setq calendar-time-zone-style 'numeric)

  (require 'solar)
  ;; Not my actual coordinates
  (setq calendar-longitude (getenv "MY_LONGITUDE"))
  (setq calendar-latitude (getenv "MY_LATITUDE"))

  (require 'cal-dst)
  ;; UTC +8. China Standard Time, Beijing Time
  (setq calendar-standard-time-zone-name "+0800")
  (setq calendar-daylight-time-zone-name "+0800"))

(use-package appt
  :ensure nil
  :commands (appt-activate)
  :config
  (require 'notifications)
  (defun appt-disp-window-and-notification (min-to-appt current-time appt-msg)
    (let ((title (format "%s 分钟内有新预约" min-to-appt)))
      (notifications-notify
       ;; Until next reminder
       :timeout (* appt-display-interval 60000)
       :title title
       :body appt-msg
       )
      ;; The original function is also called
      (appt-disp-window min-to-appt current-time appt-msg)))
  (setq appt-display-diary nil
        appt-display-mode-line t
        ;; Reminder interval
        appt-display-interval 5
        ;; Mute
        appt-audible nil
        appt-warning-time-regexp "预约 \\([0-9]+\\)"
        appt-message-warning-time 15)
  ;; Use customize func
  (setq appt-display-format 'window)
  (setq appt-disp-window-function #'appt-disp-window-and-notification)

  (with-eval-after-load 'org-agenda
    (appt-activate 1)
    (org-agenda-to-appt)))

(use-package org
  :ensure nil
  :init
  (setq org-directory "~/org-notes/")
  (setq org-imenu-depth 7)

  (add-to-list 'safe-local-variable-values '(org-hide-leading-stars . t))
  (add-to-list 'safe-local-variable-values '(org-hide-macro-markers . t))
  :mode-hydra
  ((:title "Org-Mode Commands")
   ("Main"
    ( ("l" org-insert-last-stored-link "insert last link")
      ("cl" org-toggle-link-display "toggle link display")
      ("." org-toggle-link-display "toggle link display")
      ("i" lim-org-id-headlines "id headline" :color blue)
      ("h" lim-org-ox-html "ox html" :color blue)
      )
    ))
  :bind
  ( :map global-map
    ("C-c o l" . org-store-link)
    ("C-c o a" . org-agenda)
    ("C-c o c" . org-capture)
    ("C-c o o" . org-open-at-point-global)
    :map org-mode-map
    ;; So many bindings one zillion keys and most of them useless, disable it!
    ("C-'" . nil)
    ("C-," . nil)
    ("M-;" . nil)
    ("<C-return>" . nil)
    ("<C-S-return>" . nil)
    ("C-M-S-<right>" . nil)
    ("C-M-S-<left>" . nil)
    ("C-c ;" . nil)

    ("C-c l" . org-insert-last-stored-link)
    ("C-c C-l" . org-toggle-link-display)
    ("M-." . org-edit-special)

    :map org-src-mode-map
    ("M-," . org-edit-src-exit)

    :map narrow-map
    ("b" . org-narrow-to-block)
    ("e" . org-narrow-to-element)
    ("s" . org-narrow-to-subtree)

    :map ctl-x-x-map
    ("i" . lim-org-id-headlines)
    ("h" . lim-org-ox-html))
  :config
  ;; My custom extras, which I use for the agenda and a few other Org features.
  (require 'lim-org)
  (require 'org-tempo)

  ;; general settings
  (setq org-M-RET-may-split-line '((default . nil)))
  ;; (setq org-cycle-separator-lines 0)
  (setq org-adapt-indentation nil)
  (setq org-structure-template-alist
        '(("s" . "src")
          ("e" . "src emacs-lisp")
          ("E" . "src emacs-lisp :results value code :lexical t")
          ("t" . "src emacs-lisp :tangle FILENAME")
          ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
          ("x" . "example")
          ("X" . "export")
          ("q" . "quote")))
  (setq org-catch-invisible-edits 'show)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-modules '(ol-info ol-eww))
  (setq org-use-sub-superscripts '{})
  (setq org-insert-heading-respect-content t)
  (setq org-read-date-prefer-future 'time)
  ;; other options affect elisp regexp in src blocks
  (setq org-highlight-latex-and-related '(latex))
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)
  (setq org-priority-faces nil)

  (defun lim/org-disable-angle-bracket ()
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))

  (add-hook 'org-mode-hook 'lim/org-disable-angle-bracket)

  ;; See my `pulsar' package, defined elsewhere in this setup.
  (with-eval-after-load 'pulsar
    (dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
      (add-hook hook #'pulsar-recenter-center)
      (add-hook hook #'pulsar-reveal-entry))))

(use-package org
  :ensure nil
  :config
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-reverse-note-order nil)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@/!)")
          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f!)")
          ))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#FF9966" :weight bold))
          ("DOING" . (:foreground "#6699FF" :weight bold))
          ("WAIT" . (:foreground "#333399" :weight bold))
          ("REPORT" . (:foreground "#6666CC" :weight bold))
          ("BUG" . (:foreground "#FF0066" :weight bold))
          ("KNOWNCAUSE" . (:foreground "#99CC33" :weight bold))
          ("CANCELED" . (:foreground "#FF6666" :weight bold))
          ("DONE" . (:foreground "#22AA66" :weight bold))
          ("FIXED" . (:foreground "#00AA00" :weight bold))
          ))
  (setq org-use-fast-todo-selection 'expert)

  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-whole-heading-line nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t))

(use-package org
  :ensure nil
  :config
  (setq org-tag-alist nil)
  (setq org-auto-align-tags nil)
  (setq org-tags-column 0))

(use-package org
  :ensure nil
  :config
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time))

(use-package org
  :ensure nil
  :config
  (require 'lim-org)

  (setq org-link-context-for-files t)
  (setq org-link-keep-stored-after-insertion nil)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(use-package org
  :ensure nil
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0))

(use-package org
  :ensure nil
  :init
  (setq org-export-backends '(html texinfo md))
  :config
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 8)
  (setq org-export-dispatch-use-expert-ui nil)
  (setq org-html-htmlize-output-type nil)
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil))

(use-package org
  :ensure nil
  :config
  (prettify-symbols-mode 1)
  (setq prettify-symbols-unprettify-at-point t))

(use-package display-line-numbers
  :hook (org-mode . (lambda () (display-line-numbers-mode -1))))

(use-package org-capture
  :ensure nil
  :config
  (require 'lim-org)

  (setq org-capture-templates
        `(("i" "灵感" entry
           (file+headline "inbox.org" "INSPIRE")
           ,(concat
             "* %^{Title}\n"
             ":PROPERTIES:\n"
             ":CAPTURED: %U\n"
             ":END:\n\n"
             "%i %l")
           :empty-lines 1)
          ("t" "临时" entry
           (file+headline "inbox.org" "TEMPORARY")
           ,(concat
             "* TODO [#B] %^{Title}\n"
             ":PROPERTIES:\n"
             ":EFFORT: %^{Effort estimate in minutes|5|10|15|30|45|60|90|120}\n"
             ":END:\n\n"
             "%a\n")
           :prepend t
           :clock-in t
           :clock-keep t
           :immediate-finish t
           :empty-lines 1)
          ("l" "闲时" entry
           (file+olp+datetree "leisure.org")
           ,(concat
             "* TODO [#C] %^{Title}\n"
             ":PROPERTIES:\n"
             ":CAPTURED: %U\n"
             ":END:\n\n"
             "%i%?")
           :empty-lines 1
           :tree-type month)
          ("g" "GTD" entry
           (file+olp+datetree "gtd.org")
           ,(concat
             "* TODO [#B] %^{Title} %^g\n"
             "SCHEDULED: %^t\n"
             "DEADLINE: %^t\n"
             ":PROPERTIES:\n"
             ":CAPTURED: %U\n"
             ":END:\n\n"
             "%a\n%i%?")
           :empty-lines 1
           :tree-type week)
          ))
  )

(use-package cal-china-x
  :config
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-general-holidays
                holiday-christian-holidays)))

(use-package org-agenda
  :ensure nil
  :config
  (require 'solar)
  (require 'lim-org)

  (defun lim/org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda, or timeline.

This function makes sure that dates are aligned for easy reading."
    (require 'cal-iso)
    (let* ((dayname (aref cal-china-x-days
                          (calendar-day-of-week date)))
           (day (cadr date))
           (month (car date))
           (year (nth 2 date))
           (cn-date (calendar-chinese-from-absolute (calendar-absolute-from-gregorian date)))
           (cn-month (cl-caddr cn-date))
           (cn-day (cl-cadddr cn-date))
           (cn-month-string (concat (aref cal-china-x-month-name
                                          (1- (floor cn-month)))
                                    (if (integerp cn-month)
                                        ""
                                      "(闰月)")))
           (cn-day-string (aref cal-china-x-day-name
                                (1- cn-day))))
      (format "%04d-%02d-%02d 周%s %s%s" year month
              day dayname cn-month-string cn-day-string)))

  (setq org-default-notes-file (expand-file-name ".notes" org-directory))
  (setq org-agenda-include-diary t)
  (setq diary-file (expand-file-name "standard-diary" org-directory))
  (setq org-agenda-diary-file 'diary-file)
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0300 0600 0900 1200 1500 1800 2100 2400)
          "......" "------------------------"))
  (setq org-agenda-format-date 'lim/org-agenda-format-date-aligned)

  (setq org-agenda-files `(,org-directory))
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-skip-comment-trees t)

  (setq org-agenda-scheduled-leaders
        '("Scheduled: " "Sched.%2dx: "))
  (setq org-agenda-inactive-leader "[")
  (setq org-agenda-deadline-leaders
        '("Deadline:  " "In %3d d.: " "%2d d. ago: "))

  (setq org-agenda-current-time-string "← now ──────────────────")
  )

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :config
  (setq org-modern-table nil)
  (setq org-pretty-entities t)
  (setq org-hide-emphasis-markers t)

  (setq org-modern-list
        '( ;(?+ . "➣")
          (?+ . "∙")
          (?- . "◦")
          (?* . "‣")
          ))
  )

(use-package olivetti
  :hook org-mode)

(use-package org-appear
  :hook org-modern-mode
  :config
  (setq org-appear-autolinks t))

(provide 'lim-module-org)
