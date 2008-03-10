;; general customizations

(defconst win
  (eq system-type 'windows-nt)
  "Are we running on Win32 system")

(defconst nix
  (not (eq system-type 'windows-nt))
  "Are we running on *nix system")

(defconst graf
  (not (eq window-system 'nil))
  "Are we running window system?")

;; don't ask, just do it!
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'overwrite-mode 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq
 inhibit-startup-message t           ;; don't show annoing startup msg
 make-backup-files nil               ;; NO annoing backups
 vc-follow-symlinks t                ;; follow symlinks and don't ask
 echo-keystrokes 0.01                ;; see what you type
 scroll-conservatively 50            ;; text scrolling
 scroll-preserve-screen-position nil
 scroll-margin 10
 scroll-step 1                       ;; Scroll by one line at a time
 comint-completion-addsuffix t       ;; Insert space/slash after completion
 kill-whole-line t                   ;; delete line in one stage
 default-major-mode 'text-mode       ;; default mode
 delete-key-deletes-forward t        ;; meaning are the same as the name :)
 next-line-add-newlines nil          ;; don't add new lines when scrolling down
 require-final-newline nil           ;; don't make sure file ends with NEWLINE
 delete-old-versions t               ;; delete excess backup versions
 default-tab-width 4
 mouse-yank-at-point t               ;; paste at cursor, NOT at mouse pointer position
 apropos-do-all t                    ;; apropos works better but slower
 display-time-24hr-format t
 display-time-day-and-date t
 european-calendar-style t
 calendar-week-start-day 1
 auto-save-interval 512              ;; autosave every 512 keyboard inputs
 auto-save-list-file-prefix nil
 cursor-in-non-selected-windows nil
 dired-recursive-copies 'top
 dired-recursive-deletes 'top
 safe-local-variable-values '((encoding . utf-8) (prompt-to-byte-compile))
 dabbrev-case-fold-search nil        ;; Case is significant for dabbrev
 )

(setq-default
 save-place t         ;; save position in files
 case-fold-search t   ;; case INsensitive search
 indent-tabs-mode nil ;; do not use tabs for indentation
 fill-column 72       ;; number of chars in line
)

;; Ask questions with short answers
(fset 'yes-or-no-p 'y-or-n-p)