;;; lim-hydra.el --- Lim's Key using hydra -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2024 Tabuyos

;; Author: Tabuyos <tabuyos@outlook.com>
;; Maintainer: Tabuyos <tabuyos@outlook.com>
;; Created: 2024/05/20

;;; Commentary:

;; (key-binding (kbd "RET"))
;; (key-binding (read-kbd-macro "M-x abc RET"))

;; Hydra key for Lim

;;; Code:

(require 'lim-helper)
(require 'defrepeater)
(require 'hydra)

(defvar lim-hydra-default-helpful-title "Help"
  "Default helpful title for `lim-hydra-key'.")

(defvar lim-hydra-default-quit-key '("ESC" "SPC")
  "Default quit key for `lim-hydra-key'.")

(defvar lim-hydra-default-quit-key-no-spc '("ESC")
  "Default quit key for `lim-hydra-key'.")

(defvar lim-hydra-default-quit-key-no-esc '("SPC")
  "Default quit key for `lim-hydra-key'.")

(defvar lim-hydra-default-quit-key-no-esc-spc '()
  "Default quit key for `lim-hydra-key'.")

(defvar lim-hydra-default-quit-key-only-esc '("ESC")
  "Default quit key for `lim-hydra-key'.")

(defvar lim-hydra-default-quit-key-only-spc '("SPC")
  "Default quit key for `lim-hydra-key'.")

(defvar lim-hydra-default-show-hide-key "C-h"
  "Default show hide key for `lim-hydra-key'.")

(defvar lim-hydra-default-foreign-keys 'warn
  "Default foreign-keys")

(defvar lim-hydra-icon-alpha-c (nerd-icons-mdicon "nf-md-alpha_c"))
(defvar lim-hydra-icon-alpha-g (nerd-icons-mdicon "nf-md-alpha_g"))
(defvar lim-hydra-icon-alpha-h (nerd-icons-mdicon "nf-md-alpha_h"))
(defvar lim-hydra-icon-alpha-i (nerd-icons-mdicon "nf-md-alpha_i"))
(defvar lim-hydra-icon-alpha-m (nerd-icons-mdicon "nf-md-alpha_m"))
(defvar lim-hydra-icon-alpha-q (nerd-icons-mdicon "nf-md-alpha_q"))
(defvar lim-hydra-icon-alpha-r (nerd-icons-mdicon "nf-md-alpha_r"))
(defvar lim-hydra-icon-alpha-s (nerd-icons-mdicon "nf-md-alpha_s"))
(defvar lim-hydra-icon-alpha-t (nerd-icons-mdicon "nf-md-alpha_t"))
(defvar lim-hydra-icon-alpha-x (nerd-icons-mdicon "nf-md-alpha_x"))
(defvar lim-hydra-icon-alpha-z (nerd-icons-mdicon "nf-md-alpha_z"))
(defvar lim-hydra-icon-gears (nerd-icons-faicon "nf-fa-gears"))

(defvar lim-hydra-call-chains nil
  "hydra body func call-chain, format: (from . to).

Reset chain when first-call hydra.")

(defvar lim-hydra-modeline-bg-colors nil)

(defvar lim-hydra-modeline-names nil)

(defun lim-hydra-configure (name)
  "Configure current called hydra fun.

NAME is a symbol."
  (let ((light-bg "#ffc0cb")
        ;; (dark-bg "#8470ff")
        (dark-bg "#778899")
        (bg (face-background 'mode-line-active))
        (sn (symbol-name name)))
    (push bg lim-hydra-modeline-bg-colors)
    (push sn lim-hydra-modeline-names)
    (pcase (frame--current-background-mode (selected-frame))
      ('light (set-face-background 'mode-line-active light-bg))
      ('dark (set-face-background 'mode-line-active dark-bg)))
    ))

(defun lim-hydra-unconfigure ()
  "Unconfigure hydra info."
  (let ((bg (car lim-hydra-modeline-bg-colors)))
    (set-face-background 'mode-line-active bg)
    (pop lim-hydra-modeline-bg-colors)
    (pop lim-hydra-modeline-names)
    ))

(defun lim-hydra-toggle-helpful ()
  "Toggle hydra helpful info."
  (if hydra-is-helpful (setq hydra-is-helpful nil) (setq hydra-is-helpful t)))

(defun lim-hydra-alist-remove-all (sequence keys &optional test-fn)
  "Remove all keys from sequence."
  (let (seq)
    (dolist (elt sequence)
      (unless (seq-contains-p keys (car elt) test-fn)
        (setq seq (append seq (list elt)))))
    seq))

(defun lim-hydra--run-fun (fn)
  "Run Function."
  (cond ((commandp fn)
         (call-interactively fn))
        ((and (symbolp fn) (fboundp fn))
         (funcall fn))
        ((functionp fn)
         (funcall fn))
        (t (funcall `(lambda () ,fn)))))

(defun lim-hydra--run-key (key)
  "Run Key."
  (call-interactively (key-binding (read-kbd-macro key))))

(defun lim-hydra-go-to (body-fn)
  "Call `body-fn' from current body."
  (when body-fn
    (push (cons hydra-curr-body-fn body-fn) lim-hydra-call-chains)
    (funcall body-fn)))

(defun lim-hydra-go-back (&rest fns)
  "Call these `fns' and go back to previous body fn.

Must check if the current body-fn matches,
the cdr value of the first element in the stack."
  (dolist (fn fns) (lim-hydra--run-fun fn))
  (when-let* ((pair (car lim-hydra-call-chains))
              (from (car-safe pair))
              (to (cdr-safe pair))
              (_ (equal hydra-curr-body-fn to)))
    (pop lim-hydra-call-chains)
    (call-interactively from)))

(defun lim-hydra-go-back-by-pop ()
  "Go back previous body fn by element that pop from stack."
  (interactive)
  (lim-hydra-go-back))

(defun lim-hydra-reset-chain ()
  "Reset body func call-chain."
  (interactive)
  (setq lim-hydra-call-chains nil))

(defun lim-hydra-run-remapping (command)
  "Run remapping func/self."
  (lim-hydra--run-fun (or (command-remapping command) command)))

;;;###autoload
(defun lim-hydra-next-line ()
  "Next line."
  (interactive)
  (lim-hydra-run-remapping 'next-line))

;;;###autoload
(defun lim-hydra-prev-line ()
  "Prev line."
  (interactive)
  (lim-hydra-run-remapping 'previous-line))

;;;###autoload
(defun lim-hydra-enter ()
  "Enter."
  (interactive)
  (lim-hydra--run-key "RET"))

;;;###autoload
(defun lim-hydra-tab ()
  "Enter."
  (interactive)
  (lim-hydra--run-key "TAB"))

;;;###autoload
(defun lim-hydra-yank ()
  "Yank."
  (interactive)
  (lim-hydra--run-key "C-y"))

;;;###autoload
(defun lim-hydra-save ()
  "Enter."
  (interactive)
  (lim-hydra-run-remapping 'save-buffer))

(defmacro lim-hydra-pretty-define (name body heads-plist)
  (declare (indent defun))
  (let* ((show-hide-key (or (plist-get body :show-hide-key) lim-hydra-default-show-hide-key))
         (foreign-keys (or (plist-get body :foreign-keys) lim-hydra-default-foreign-keys))
         (quit-key (or (plist-get body :quit-key) lim-hydra-default-quit-key))
         (body-pre (plist-get body :body-pre))
         (post (plist-get body :post))
         (separator (or (plist-get body :separator) "-"))
         (no-help (plist-get body :no-help))
         (no-config (plist-get body :no-config))

         (body-pre (if no-config body-pre `(progn (lim-hydra-configure ',name) ,body-pre)))
         (post (if no-config post `(progn (lim-hydra-unconfigure) ,post)))

         (new-body (if show-hide-key (lim-helper-plist-delete body :show-hide-key) body))
         (new-body (if no-help (lim-helper-plist-delete body :no-help) body))
         (new-body (if no-config (lim-helper-plist-delete body :no-config) body))

         (new-body (plist-put new-body :foreign-keys foreign-keys))
         (new-body (plist-put new-body :quit-key quit-key))
         (new-body (plist-put new-body :body-pre body-pre))
         (new-body (plist-put new-body :post post))
         (new-body (plist-put new-body :separator separator))

         (new-heads (let* ((heads (eval heads-plist))
                           (helpful (plist-get heads lim-hydra-default-helpful-title 'string=)))
                      (unless no-help
                        (plist-put
                         heads
                         lim-hydra-default-helpful-title
                         (append helpful
                                 `((,show-hide-key (lim-hydra-toggle-helpful) "helpful")))
                         'string=))
                      heads)))
    `(pretty-hydra-define ,name ,new-body ,new-heads)))

(defvar lim-hydra-entries
  '( :-> (forward-char "->")
     :-< (backward-char "<-")
     :bol (crux-move-beginning-of-line "|<-")
     :ori-bol (move-beginning-of-line "|<-")
     :eol (move-end-of-line "->|")
     :bob (beginning-of-buffer "ToB")
     :eob (end-of-buffer "ToE")
     :->> (forward-word "->>")
     :-<< (backward-word "<<-")
     :-^ (lim-hydra-prev-line "-^")
     :-v (lim-hydra-next-line "-v")
     :ori-^ (previous-line "-^")
     :ori-v (next-line "-v")
     :jump (avy-goto-char-timer "Jump")
     :ctb (recenter-top-bottom "C-T-B")
     :del-> (delete-forward-char "Del ->")
     :del-< (delete-backward-char "Del <-")
     :e-del-< (backward-delete-char-untabify "Del <-")
     :del->> (kill-word "Del ->>")
     :del-<< (backward-kill-word "Del <<-")
     :kwl/r (lim-simple-kill-whole-line-or-region "KWL/R")
     :dl/r (lim-simple-duplicate-line-or-region "DL/R")
     :format (format-all-region-or-buffer "Format")
     :kill-line (kill-line "Kill Line")
     :newline (crux-smart-open-line "Newline")
     :newline-above (crux-smart-open-line-above "Newline Above")
     :join-line (crux-top-join-line "Join Line")
     :undo (undo "Undo")
     :v-undo (vundo "V-Undo")
     :n-page (scroll-up-command "Next Page")
     :p-page (scroll-down-command "Prev Page")
     :mark (set-mark-command "Set Mark")
     :mark-all (mark-whole-buffer "Mark All")
     :c-mark (lim-simple-deactivate-mark "Cancel Mark")
     :x-mark (exchange-point-and-mark "Point <-> Mark")
     :copy (kill-ring-save "Copy")
     :cut (kill-region "Cut")
     :yank (yank "Yank")
     :yank-pop (consult-yank-pop "Yank Pop")
     :f-sexp (forward-sexp "F Sexp")
     :b-sexp (backward-sexp "B Sexp")
     :eval-expr (eval-expression "Eval Expr")
     :mmh (major-mode-hydra "M-M-H")
     :insert-comma (lim-simple-insert-comma ",")
     :insert-space (lim-simple-insert-space " ")
     :insert-comma-space (lim-simple-insert-comma-space ", ")
     :command (execute-extended-command "Command")
     :compile (compile "Compile")
     :save (lim-hydra-save "Save")
     :ori-save (save-buffer "Save")
     :save-other (save-some-buffers "Save Other")
     :rename (rename-buffer "Rename")
     :kill (kill-buffer "Kill")
     :kill-other (kill-matching-buffers "Kill Other")
     :kill-current (kill-current-buffer "Kill Current")
     :go-back (lim-hydra-go-back-by-pop "Go Back")
     :re-buffer (revert-buffer "Revert Buffer")
     :quit (lim-simple-quit "Quit")
     :kb-quit (keyboard-quit "Keyboard Quit")
     :mb-kb-quit (minibuffer-keyboard-quit "Minibuffer Quit")
     :enter (lim-hydra-enter "Enter")
     :tab (lim-hydra-tab "TAB")
     :capital (capitalize-dwim "Capitalize")
     :upcase (upcase-dwim "Up Case")
     :downcase (downcase-dwim "Down Case")
     :m-scratch (lim-scratch-buffer "Major Scratch")
     :prev-buffer (previous-buffer "Prev Buffer")
     :next-buffer (next-buffer "Next Buffer")
     :pop-to-buffer (pop-to-buffer "Pop Buffer")

     :move-up (move-dup-move-lines-up "Move Up")
     :move-down (move-dup-move-lines-down "Move Down")

     :mouse-set-point (mouse-set-point "Point")
     :mouse-drag-region (mouse-drag-region "Drag Region")
     :mouse-set-region (mouse-set-region "Set Region")
     :mwheel-scroll (mwheel-scroll "Point")
     
     :hs-toggle (hs-toggle-hiding "HS Toggle")
     :hs-hide-all (hs-hide-all "HS Hide All")
     :hs-show-all (hs-show-all "HS Show All")

     :dired-sidebar-toggle (dired-sidebar-toggle-sidebar "Dired")
     :dired-sidebar-jump (dired-sidebar-jump-to-sidebar "Jump Dired")
     :dired-sidebar-show-selected-file (lim-simple-dired-sidebar-show-selected-file "Show File")

     :e-expand (expreg-expand "Expand")
     :e-contract (expreg-contract "Contract")

     :vertico-next (vertico-next "Next")
     :vertico-prev (vertico-previous "Prev")
     :vertico-enter (vertico-directory-enter "Enter")

     :xref-find-def (xref-find-definitions "Find Def")
     :xref-find-ref (xref-find-references "Find Ref")
     :xref-go-back (xref-go-back "Go Backward")
     :xref-go-for (xref-go-forward "Go Forward")

     :view-mode (view-mode "View")
     :ws-mode (whitespace-mode "Whitespace")
     :wsc-mode (whitespace-cleanup-mode "Whitespace Cleanup")
     :rb-mode (rainbow-mode "Rainbow")
     :fm-mode (flymake-mode "Flymake")
     :repeat-mode (repeat-mode "Repeat")
     :winner-mode (winner-mode "Winner")

     :lim-search-mode (lim-search-mode "Lim's Search")

     :search (isearch-forward "Search")
     :search-symbol (isearch-forward-symbol-at-point "Search Symbol")
     :search-thing (isearch-forward-thing-at-point "Search Thing")
     :search-edit (isearch-edit-string "Edit")
     :search-exit (isearch-exit "Exit")
     :search-abort (isearch-abort "Abort")
     :search-repeat-f (isearch-repeat-forward "Forward Search")
     :search-repeat-b (isearch-repeat-backward "Backward Search")

     :ai-gptel (gptel "Interactive Chat")
     :ai-gptel-send (gptel-send "Send The Line")

     :describe-keymap (describe-keymap "Describe Keymap")
     :describe-face (describe-face "Describe Face")
     :describe-prefix-bindings (describe-prefix-bindings "Describe Prefix")
     :describe-key (describe-key "Describe Key")
     :describe-mode (describe-mode "Describe Mode")
     :describe-char (describe-char "Describe Char")
     :describe-function (describe-function "Describe Function")
     :describe-variable (describe-variable "Describe Variable")

     :cap (completion-at-point "Completion")
     :cap-tag (complete-tag "Complete Tag")

     :so-put (symbol-overlay-put "Put SO")
     :so-remove-all (symbol-overlay-remove-all "RM SO")

     :project-switch (project-switch-project "Switch Project")
     :project-switch-buffer (project-switch-to-buffer "Switch Buffer")
     :project-forget (project-forget-project "Forget Project")
     :project-find (project-find-file "Find File")
     :project-compile (project-compile "Compile")

     :open-file (find-file "Open File")
     :open-recent (recentf-open "Recent Open")

     :consult-x (consult-mode-command "Command")
     :consult-search (consult-isearch-history "Search")
     :consult-buffer (consult-buffer "Buffer")
     :consult-project-buffer (consult-project-buffer "Project Buffer")
     :consult-bookmark (consult-bookmark "Bookmark")
     :consult-man (consult-man "Man")
     :consult-goto-line (consult-goto-line "Go to Line")
     :consult-outline (consult-outline "Outline")
     :consult-mark (consult-mark "Go to Mark")
     :consult-imenu (consult-imenu "IMenu")
     :consult-find (consult-find "Find")
     :consult-grep (consult-grep "Grep")
     :consult-ripgrep (consult-ripgrep "RG")
     :consult-line (consult-line "Search Line")
     :consult-git-grep (consult-git-grep "Git Grep")
     :consult-locate (consult-locate "Locate")

     :embark-act (embark-act "Action")
     :embark-dwim (embark-dwim "Dwim")
     :embark-bindings (embark-bindings "Bindings")
     :embark-collect (embark-collect "Collect")
     :embark-export (embark-export "Export")

     :mc-mark-point (lim-simple-mc-mark-at-point "Mark Point")
     :mc-activate (lim-simple-mc-activate "Activate")
     :mc-edit-lines (mc/edit-lines "Edit Lines")
     :mc-edit-eol (mc/edit-ends-of-lines "Edit End of Lines")
     :mc-edit-bol (mc/edit-beginnings-of-lines "Edit Beg of Lines")
     :mc-mark-nl (mc/mark-next-like-this "Mark Next Line")
     :mc-mark-pl (mc/mark-previous-like-this "Mark Prev Line")
     :mc-mark-inregion (mc/mark-all-like-this "Mark in Region")
     :mc-mark-dwim (mc/mark-all-like-this-dwim "Mark in Dwim")
     :mc-mark-infunc (mc/mark-all-like-this-in-defun "Mark in Func")

     :window-switch (other-window "Switch")
     :window-delete (delete-window "Delete")
     :window-delete-other (delete-other-windows "Delete Other")
     :window-ace (ace-window "Ace")
     :window-ace-select (ace-select-window "Ace Select")
     :window-ace-swap (ace-swap-window "Ace Swap")

     :window-swap-left (windmove-swap-states-left "Swap Left")
     :window-swap-right (windmove-swap-states-right "Swap Right")
     :window-swap-down (windmove-swap-states-down "Swap Down")
     :window-swap-up (windmove-swap-states-up "Swap Up")

     :window-move-left (windmove-left "Move Left")
     :window-move-right (windmove-right "Move Right")
     :window-move-down (windmove-down "Move Down")
     :window-move-up (windmove-up "Move Up")

     :window-resize-eh (enlarge-window-horizontally "Enlarge H")
     :window-resize-sh (shrink-window-horizontally "Shrink H")
     :window-resize-ev (enlarge-window "Enlarge V")
     :window-resize-sv (shrink-window "Shrink V")
     :window-resize-b (balance-windows "Balance")

     :window-split-right (split-window-right "Split Right")
     :window-split-below (split-window-below "Split Below")
     :window-split-right-root (split-root-window-right "Split Right(Root)")
     :window-split-below-root (split-root-window-below "Split Below(Root)")

     :winner-redo (winner-redo "Redo")
     :winner-undo (winner-undo "Undo")


     :root ((limhydragt 'lim-hydra/body) "Root")


     :body-c ((limhydragt 'lim-hydra-c/body) "(C)offee")
     :body-g ((limhydragt 'lim-hydra-g/body) "(G)olden")
     :body-h ((limhydragt 'lim-hydra-h/body) "(H)appy")
     :body-m ((limhydragt 'lim-hydra-m/body) "(M)ore")
     :body-i ((limhydragt 'lim-hydra-i/body) "(I)ce")
     :body-q ((limhydragt 'lim-hydra-q/body) "(Q)uick")
     :body-r ((limhydragt 'lim-hydra-r/body) "(R)ecipe")
     :body-s ((limhydragt 'lim-hydra-s/body) "(S)atan")
     :body-t ((limhydragt 'lim-hydra-t/body) "(T)oggle")
     :body-x ((limhydragt 'lim-hydra-x/body) "(X)")
     :body-z ((limhydragt 'lim-hydra-z/body) "(Z)eta")

     :body-c-cap ((limhydragt 'lim-hydra-c-cap/body) "C-A-P")
     
     :body-s-es ((limhydragt 'lim-hydra-s-es/body) "Enhance Search")
     :body-s-find ((limhydragt 'lim-hydra-s-find/body) "Find")
     
     :body-m-ai ((limhydragt 'lim-hydra-m-ai/body) "AI")
     :body-m-consult ((limhydragt 'lim-hydra-m-consult/body) "Consult Navigate")
     :body-m-describe ((limhydragt 'lim-hydra-m-describe/body) "Describe")
     :body-m-mc ((limhydragt 'lim-hydra-m-mc/body) "Multi Cursor")
     :body-m-visual ((limhydragt 'lim-hydra-m-visual/body) "Visual")
     :body-m-visual-vertico ((limhydragt 'lim-hydra-m-visual-vertico/body) "Vertico")
     :body-m-buffer ((limhydragt 'lim-hydra-m-buffer/body) "Buffer")
     :body-m-project ((limhydragt 'lim-hydra-m-project/body) "Project")
     :body-m-embark ((limhydragt 'lim-hydra-m-embark/body) "Embark")
     :body-m-open ((limhydragt 'lim-hydra-m-open/body) "Open")

     :body-m-window ((limhydragt 'lim-hydra-m-window/body) "Window Management")
     :body-m-window-resize ((limhydragt 'lim-hydra-m-window-resize/body) "Window Resize")
     :body-m-window-split ((limhydragt 'lim-hydra-m-window-split/body) "Window Split")
     )
  "Hydra entries table.")

(defun lim-hydra-build (prop key &rest args)
  "Build Entry."
  (apply #'lim-hydra--build prop key nil args))

(defun lim-hydra-build-back (prop key &rest args)
  "Build Entry."
  (apply #'lim-hydra--build prop key t args))

(defun lim-hydra--build (prop key back &rest args)
  "Build Entry."
  (let ((entry (plist-get lim-hydra-entries prop)))
    (unless entry (user-error "not found %s" prop))
    (if back
        `(,key (lim-hydra-go-back ',(car entry)) ,(cadr entry) ,@args)
      `(,key ,@entry ,@args))))

(defalias 'limhydrab 'lim-hydra-build)
(defalias 'limhydrabk 'lim-hydra-build-back)
(defalias 'limhydragt 'lim-hydra-go-to)
(defalias 'limhydragb 'lim-hydra-go-back)

(lim-hydra-pretty-define lim-hydra
  ( :title (s-concat lim-hydra-icon-gears " My Key Bindings")
    :body-pre (lim-hydra-reset-chain)
    :foreign-keys run
    :quit-key ("SPC"))
  `("Moving"
    (,(limhydrab :bol "a")
     ,(limhydrab :eol "e")
     ,(limhydrab :-< "b")
     ,(limhydrab :-> "f")
     ,(limhydrab :-v "n")
     ,(limhydrab :-^ "p")
     ,(limhydrab :ctb "l")
     ,(limhydrab :n-page "v")
     ,(limhydrab :b-sexp "[")
     ,(limhydrab :f-sexp "]")
     ,(limhydrab :jump ";" :exit t))

    "S-Moving"
    (,(limhydrab :bob "A")
     ,(limhydrab :eob "E")
     ,(limhydrab :-<< "B")
     ,(limhydrab :->> "F")
     ,(limhydrab :p-page "V"))

    "Broken"
    (,(limhydrab :del-> "d")
     ,(limhydrab :e-del-< "DEL")
     ,(limhydrab :del->> "D")
     ,(limhydrab :del-<< "C-<backspace>")
     ,(limhydrab :newline "j")
     ,(limhydrab :join-line "J")
     ,(limhydrab :kill-line "k")
     ,(limhydrab :newline-above "o")
     ,(limhydrab :v-undo "u" :exit t)
     ,(limhydrab :move-up "P")
     ,(limhydrab :move-down "N")
     ,(limhydrab :undo "/"))

    "Go"
    (,(limhydrab :xref-find-def "M-.")
     ,(limhydrab :xref-find-ref "C-M-.")
     ,(limhydrab :xref-go-back "M-,")
     ,(limhydrab :xref-go-for "C-M-,")
     ,(limhydrab :hs-toggle "C-."))

    "C/V"
    (,(limhydrab :copy "w")
     ,(limhydrab :yank "y")
     ,(limhydrab :yank-pop "Y"))

    "To"
    (,(limhydrab :body-c "c" :exit t)
     ,(limhydrab :body-g "g" :exit t)
     ,(limhydrab :body-h "h" :exit t)
     ,(limhydrab :body-i "i" :exit t)
     ,(limhydrab :body-m "m" :exit t)
     ,(limhydrab :body-q "q" :exit t))

    "To"
    (,(limhydrab :body-r "r" :exit t)
     ,(limhydrab :body-s "s" :exit t)
     ,(limhydrab :body-t "t" :exit t)
     ,(limhydrab :body-x "x" :exit t)
     ,(limhydrab :body-z "z" :exit t))

    "Help"
    (,(limhydrab :insert-comma "," :exit t)
     ,(limhydrab :mmh "." :exit t)
     ,(limhydrab :tab "TAB" :exit t)
     ,(limhydrab :enter "RET" :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-c
  (:title "Coffee")
  `("Latte"
    (,(limhydrab :command "c" :exit t)
     ,(limhydrab :compile "C" :exit t)
     ,(limhydrabk :dl/r "d" :exit t)
     ,(limhydrabk :format "f" :exit t)
     ,(limhydrabk :format "l" :exit t)
     ,(limhydrabk :kwl/r "k" :exit t))

    "Switch"
    (,(limhydrab :consult-buffer "e" :exit t))

    "Cappuccino"
    (,(limhydrab :save "s" :exit t)
     ,(limhydrab :save-other "S" :exit t))

    "Mocha"
    (,(limhydrab :eval-expr "x" :exit t)
     ,(limhydrab :cap "TAB" :exit t))

    "To"
    (,(limhydrab :body-c-cap "p" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-g
  (:title "Golden")
  `("Copilot"
    (,(limhydrab :quit "g" :exit t)
     ,(limhydrab :kb-quit "G" :exit t)
     ,(limhydrab :mb-kb-quit "m" :exit t)
     ,(limhydrab :re-buffer "r" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-h
  (:title "Happy")
  `("Dance"
    (,(limhydrab :mark-all "h" :exit t)
     ,(limhydrab :hs-hide-all "f")
     ,(limhydrab :hs-show-all "s")
     ,(limhydrab :ctb "l"))
    
    "To"
    (,(limhydrab :body-c "c" :exit t)
     ,(limhydrab :body-g "g" :exit t)
     ,(limhydrab :body-m "m" :exit t)
     ,(limhydrab :body-q "q" :exit t)
     ,(limhydrab :body-r "r" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-i
  (:title "Ice")
  `("Cream"
    (,(limhydrab :bol "a")
     ,(limhydrab :eol "e")
     ,(limhydrab :-< "b")
     ,(limhydrab :-> "f")
     ,(limhydrab :-v "n")
     ,(limhydrab :-^ "p")
     ,(limhydrab :ctb "l")
     ,(limhydrab :n-page "v")
     ,(limhydrab :b-sexp "[")
     ,(limhydrab :f-sexp "]"))

    "Gelato"
    (,(limhydrab :bob "A")
     ,(limhydrab :eob "E")
     ,(limhydrab :-<< "B")
     ,(limhydrab :-<< "u")
     ,(limhydrab :-<< "J")
     ,(limhydrab :->> "F")
     ,(limhydrab :->> "o")
     ,(limhydrab :->> "K")
     ,(limhydrab :-< "j")
     ,(limhydrab :-> "k")
     ,(limhydrab :-v "m")
     ,(limhydrab :-^ "i")
     ,(limhydrab :p-page "V"))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m
  (:title "More" :quit-key ("ESC"))
  `("Moon"
    (,(limhydrabk :x-mark "x" :exit t)
     ,(limhydrabk :c-mark "z" :exit t)
     ,(limhydrabk :mark "SPC" :exit t))
    
    "To"
    (,(limhydrab :body-m-ai "a" :exit t)
     ,(limhydrab :body-m-buffer "b" :exit t)
     ,(limhydrab :body-m-consult "c" :exit t)
     ,(limhydrab :body-m-describe "d" :exit t)
     ,(limhydrab :body-m-embark "e" :exit t)
     ,(limhydrab :body-m-mc "m" :exit t))

    "To"
    (,(limhydrab :body-m-open "o" :exit t)
     ,(limhydrab :body-m-project "p" :exit t)
     ,(limhydrab :body-q "q" :exit t)
     ,(limhydrab :body-m-visual "v" :exit t)
     ,(limhydrab :body-m-window "w" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-q
  (:title "Quick" :quit-key ("ESC"))
  `("C1"
    (,(limhydrab :bol "a")
     ,(limhydrab :eol "e")
     ,(limhydrab :bob "A")
     ,(limhydrab :eob "E")
     ,(limhydrab :-< "b")
     ,(limhydrab :-> "f")
     ,(limhydrab :-<< "B")
     ,(limhydrab :->> "F")
     ,(limhydrab :del-> "d")
     ,(limhydrab :e-del-< "DEL")
     ,(limhydrab :del->> "D")
     ,(limhydrab :del-<< "C-<backspace>")
     ,(limhydrab :capital "g")
     ,(limhydrab :downcase "i"))

    "C2"
    (,(limhydrab :upcase "I")
     ,(limhydrab :newline "j")
     ,(limhydrab :join-line "J")
     ,(limhydrab :kill-line "k")
     ,(limhydrab :ctb "l")
     ,(limhydrab :-v "n")
     ,(limhydrab :-^ "p")
     ,(limhydrab :kb-quit "q")
     ,(limhydrab :search "s")
     ,(limhydrab :v-undo "u"))

    "C3"
    (,(limhydrab :n-page "v")
     ,(limhydrab :p-page "V")
     ,(limhydrab :copy "w")
     ,(limhydrab :yank "y")
     ,(limhydrab :yank-pop "Y")
     ,(limhydrab :c-mark "z")
     ,(limhydrab :mark "SPC")
     ,(limhydrab :undo "/")
     ,(limhydrab :b-sexp "[")
     ,(limhydrab :f-sexp "]"))

    "To"
    (,(limhydrab :body-c "c" :exit t)
     ,(limhydrab :body-h "h" :exit t)
     ,(limhydrab :body-m "m" :exit t)
     ,(limhydrab :body-r "r" :exit t)
     ,(limhydrab :body-t "t" :exit t)
     ,(limhydrab :body-x "x" :exit t))
    
    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-r
  (:title "Recipe" :quit-key ("ESC" "g"))
  `("Radio"
    (,(limhydrab :bol "a")
     ,(limhydrab :eol "e")
     ,(limhydrab :bob "A")
     ,(limhydrab :eob "E")
     ,(limhydrab :-< "b")
     ,(limhydrab :-> "f")
     ,(limhydrab :-<< "B")
     ,(limhydrab :->> "F"))

    "Rain"
    (,(limhydrab :ctb "l")
     ,(limhydrabk :view-mode "r" :toggle 'buffer-read-only :exit t)
     ,(limhydrab :search "s" :exit t)
     ,(limhydrab :n-page "v")
     ,(limhydrab :p-page "V")
     ,(limhydrab :copy "w"))

    "Raise"
    (,(limhydrab :c-mark "z")
     ,(limhydrab :mark "SPC")
     ,(limhydrab :b-sexp "[")
     ,(limhydrab :f-sexp "]"))

    "To"
    (,(limhydrab :body-c "c" :exit t)
     ,(limhydrab :body-h "h" :exit t)
     ,(limhydrab :body-i "i" :exit t)
     ,(limhydrab :body-m "m" :exit t)
     ,(limhydrab :body-q "q" :exit t)
     ,(limhydrab :body-t "t" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-s
  (:title "Satan")
  `("Satiny"
    (,(limhydrab :search "s" :exit t)
     ,(limhydrab :search-edit "e")
     ,(limhydrab :search-symbol "m" :exit t)
     ,(limhydrab :search-thing "t" :exit t)
     ,(limhydrab :lim-search-mode "N" :toggle t :exit t))

    "Symbol"
    (,(limhydrab :so-put "p" :exit t)
     ,(limhydrab :so-remove-all "r" :exit t))

    "To"
    (,(limhydrab :body-s-es "n" :exit t)
     ,(limhydrab :body-s-find "f" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-t
  (:title "Toggle")
  `("M1"
    (,(limhydrab :view-mode "vv" :toggle t)
     ,(limhydrab :ws-mode "ws" :toggle t)
     ,(limhydrab :wsc-mode "wc" :toggle t)
     ,(limhydrab :rb-mode "rb" :toggle t)
     ,(limhydrab :fm-mode "fm" :toggle t))

    "M2"
    (,(limhydrab :repeat-mode "rp" :toggle t)
     ,(limhydrab :winner-mode "wi" :toggle t))

    "Less is More"
    (,(limhydrab :lim-search-mode "ls" :toggle t :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-x
  (:title "X")
  `("oXygen"
    (,(limhydrab :save "s" :exit t)
     ,(limhydrab :save-other "S" :exit t)
     ,(limhydrabk :cut "x" :exit t))

    "C/V"
    (,(limhydrab :copy "w")
     ,(limhydrab :yank "y")
     ,(limhydrab :yank-pop "Y"))

    "Moving"
    (,(limhydrab :-< "b")
     ,(limhydrab :-> "f")
     ,(limhydrab :-v "n")
     ,(limhydrab :-^ "p"))

    "eXpreg"
    (,(limhydrab :e-expand "e")
     ,(limhydrab :e-contract "c"))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-z
  (:title "Zeta")
  `("Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m-window
  (:title "Window Management")
  `("Switch"
    (,(limhydrab :window-switch "TAB")
     ,(limhydrab :window-delete "d")
     ,(limhydrab :window-delete-other "D")
     ,(limhydrab :window-ace "o" :exit t)
     ,(limhydrab :window-ace-select "a"))

    "Swap"
    (,(limhydrab :window-ace-swap "S")
     ,(limhydrab :window-swap-left "J")
     ,(limhydrab :window-swap-right "K")
     
     ,(limhydrab :window-swap-up "I"))

    "Moving"
    (,(limhydrab :window-move-left "j")
     ,(limhydrab :window-move-right "k")
     ,(limhydrab :window-move-down "m")
     ,(limhydrab :window-move-up "i"))

    "Winner"
    (,(limhydrab :winner-redo "[")
     ,(limhydrab :winner-undo "]"))

    "POP"
    (,(limhydrab :pop-to-buffer "p" :exit t))

    "To"
    (,(limhydrab :body-m-window-resize "r" :exit t)
     ,(limhydrab :body-m-window-split "s" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m-window-resize
  (:title "Window Resize")
  `("Horizontally"
    (,(limhydrab :window-resize-sh "j")
     ,(limhydrab :window-resize-eh "k"))

    "Vertically"
    (,(limhydrab :window-resize-sv "m")
     ,(limhydrab :window-resize-ev "i"))

    "Balance"
    (,(limhydrab :window-resize-b "b"))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m-window-split
  (:title "Window Split")
  `("Horizontally"
    (,(limhydrab :window-split-right "k")
     ,(limhydrab :window-split-right-root "K"))

    "Vertically"
    (,(limhydrab :window-split-below "m")
     ,(limhydrab :window-split-below-root "M"))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m-ai
  (:title "AI Collection")
  `("GPT"
    (,(limhydrab :ai-gptel "c" :exit t)
     ,(limhydrab :ai-gptel-send "s" :exit t))
    
    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m-consult
  (:title "Consult Tool")
  `("Sweet"
    (,(limhydrab :consult-x "x" :exit t)
     ,(limhydrab :consult-man "m" :exit t)
     ,(limhydrab :consult-search "s" :exit t)
     ,(limhydrab :consult-buffer "b" :exit t)
     ,(limhydrab :consult-project-buffer "B" :exit t)
     ,(limhydrab :consult-bookmark "k" :exit t)
     ,(limhydrab :consult-goto-line "t" :exit t)
     ,(limhydrab :consult-outline "o" :exit t)
     ,(limhydrab :consult-mark "a" :exit t))

    "Lookup"
    (,(limhydrab :consult-outline "o" :exit t)
     ,(limhydrab :consult-imenu "i" :exit t)
     ,(limhydrab :consult-find "f" :exit t)
     ,(limhydrab :consult-grep "g" :exit t)
     ,(limhydrab :consult-git-grep "G" :exit t)
     ,(limhydrab :consult-ripgrep "r" :exit t)
     ,(limhydrab :consult-line "l" :exit t)
     ,(limhydrab :consult-locate "L" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m-mc
  (:title "Multi Cursor")
  `("Mark"
    (,(limhydrab :mc-mark-inregion "mt")
     ,(limhydrab :mc-mark-dwim "mm")
     ,(limhydrab :mc-mark-infunc "md")
     ,(limhydrab :mc-mark-point "mg")
     ,(limhydrab :mc-mark-nl "mn")
     ,(limhydrab :mc-mark-pl "mp"))

    "Edit"
    (,(limhydrab :mc-edit-lines "ml" :exit t)
     ,(limhydrab :mc-edit-bol "ma" :exit t)
     ,(limhydrab :mc-edit-eol "me" :exit t)
     ,(limhydrab :mc-activate "mh" :exit t))

    "Moving"
    (,(limhydrab :bol "a")
     ,(limhydrab :eol "e")
     ,(limhydrab :-< "b")
     ,(limhydrab :-> "f")
     ,(limhydrab :-v "n")
     ,(limhydrab :-^ "p")
     ,(limhydrab :ctb "l")
     ,(limhydrab :n-page "v")
     ,(limhydrab :b-sexp "[")
     ,(limhydrab :f-sexp "]")
     ,(limhydrab :jump ";" :exit t))

    "S-Moving"
    (,(limhydrab :bob "A")
     ,(limhydrab :eob "E")
     ,(limhydrab :-<< "B")
     ,(limhydrab :->> "F")
     ,(limhydrab :p-page "V"))

    "To"
    (,(limhydrab :body-i "i" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m-describe
  (:title "Describe")
  `("Dance"
    (,(limhydrab :describe-face "a" :exit t)
     ,(limhydrab :describe-char "c" :exit t)
     ,(limhydrab :describe-function "f" :exit t)
     ,(limhydrab :describe-keymap "e" :exit t)
     ,(limhydrab :describe-key "k" :exit t)
     ,(limhydrab :describe-mode "m" :exit t)
     ,(limhydrab :describe-prefix-bindings "p" :exit t)
     ,(limhydrab :describe-variable "v" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m-visual
  (:title "Visual")
  `("To"
    (,(limhydrab :body-m-visual-vertico "v" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m-visual-vertico
  (:title "Vertico")
  `("Moving"
    (,(limhydrab :vertico-next "n")
     ,(limhydrab :vertico-prev "p"))

    "In"
    (,(limhydrab :vertico-enter "RET" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m-buffer
  (:title "Buffer")
  `("Scratch"
    (,(limhydrab :m-scratch "s" :exit t))

    "Switch"
    (,(limhydrab :consult-buffer "b" :exit t)
     ,(limhydrab :consult-project-buffer "B" :exit t))

    "Navigation"
    (,(limhydrab :prev-buffer "p")
     ,(limhydrab :next-buffer "n"))

    "Operate"
    (,(limhydrab :kill "k" :exit t)
     ,(limhydrab :kill-current "K" :exit t)
     ,(limhydrab :rename "r" :exit t))
    
    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m-project
  (:title "Project")
  `("Management"
    (,(limhydrab :project-switch "p" :exit t)
     ,(limhydrab :project-switch-buffer "b" :exit t)
     ,(limhydrab :project-compile "c" :exit t)
     ,(limhydrab :project-forget "d" :exit t)
     ,(limhydrab :project-find "f" :exit t))

    "Dired"
    (,(limhydrab :dired-sidebar-toggle "e" :exit t)
     ,(limhydrab :dired-sidebar-show-selected-file "s" :exit t)
     ,(limhydrab :dired-sidebar-jump "g" :exit t))
    
    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m-embark
  (:title "Embark")
  `("Management"
    (,(limhydrab :embark-act "a" :exit t)
     ,(limhydrab :embark-bindings "B" :exit t)
     ,(limhydrab :embark-collect "c" :exit t)
     ,(limhydrab :embark-export "e" :exit t)
     ,(limhydrab :embark-dwim ";" :exit t))
    
    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-m-open
  (:title "Open Any")
  `("File"
    (,(limhydrab :open-file "f" :exit t)
     ,(limhydrab :open-recent "r" :exit t))

    "Project"
    (,(limhydrab :project-switch "p" :exit t))
    
    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-s-es
  (:title "Enhance Search")
  `("Easy"
    (,(limhydrab :search-repeat-f "n")
     ,(limhydrab :search-repeat-b "p")
     ,(limhydrab :search-abort "g" :exit t)
     ,(limhydrab :search-exit "<escape>" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-s-find
  (:title "Find" :body-pre (progn (isearch-forward nil t) (isearch-edit-string)))
  `("Fire"
    (,(limhydrab :search-repeat-f "n")
     ,(limhydrab :search-repeat-b "p")
     ,(limhydrab :search-edit "e")
     ,(limhydrab :search-abort "g" :exit t)
     ,(limhydrab :search-exit "<escape>" :exit t))

    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(lim-hydra-pretty-define lim-hydra-c-cap
  (:title "Completion at Point")
  `("Management"
    (,(limhydrab :cap "p" :exit t)
     ,(limhydrab :cap-tag "t" :exit t))
    
    "Help"
    (,(limhydrab :root "," :exit t)
     ,(limhydrab :go-back "." :exit t))
    ))

(provide 'lim-hydra)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
