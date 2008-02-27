;; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU Emacs config
;; (c) Alexander Solovyov 2004-2007
;; piranha AT piranha.org.ua
;;
;; Thanks to all, who has helped me in creation, especially to:
;; Yuriy Sazonets
;; Alex Ott
;; Emacswiki.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; systems
;;;;;;;;;;

(defconst win32
  (eq system-type 'windows-nt)
  "Are we running on Win32 system")

(defconst linux
  (eq system-type 'gnu/linux)
  "Are we running on linux system")

(defconst graf
  (not (eq window-system 'nil))
  "Are we running window system?")


;;;;;;;;;;
;; lang setup & changing with system switcher
;;;;;;;;;;

(setq default-input-method "russian-computer")

(when linux
  (set-selection-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq
   x-select-enable-clipboard t
   x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
   interprogram-paste-function (quote x-cut-buffer-or-selection-value)
   ))

(prefer-coding-system 'utf-8-unix)

;;;;;;;;;;;;;
;; Extensions
;;;;;;;;;;;;;

;; loadpath
(add-to-list 'load-path (expand-file-name "~/.el"))

(require 'gnus-load nil t)
(require 'filladapt nil t)
(require 'session nil t)
(require 'htmlize nil t)
(require 'django-html-mode nil t)
(require 'grep+ nil t)
(require 'mercurial nil t)
(require 'etags nil t)
(setq tags-file-name (expand-file-name "~/TAGS"))

;; Use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

;;;;;;;;;;
;; General

(add-hook 'after-init-hook 'session-initialize)

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
 scroll-preserve-screen-position 't
 scroll-margin 10
 scroll-step 1                       ;; Scroll by one line at a time
 comint-completion-addsuffix t       ;; Insert space/slash after completion
 fill-column 72                      ;; number of chars in line
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
 safe-local-variable-values '((encoding . utf-8))
)

(setq-default
 save-place t         ;; save position in files
 case-fold-search t   ;; case INsensitive search
 indent-tabs-mode nil ;; do not use tabs for indentation
)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; no blinking cursor
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

;; display time
(display-time)

;; This tells emacs to show the column number in each modeline.
(column-number-mode 1)

;; highlight marked text
(transient-mark-mode 1)

;; to highlight ( and )
(show-paren-mode 1)

;; Turn off this idiotic stickyfunc mode
(if (boundp 'global-semantic-stickyfunc-mode)
    (global-semantic-stickyfunc-mode -1))

;; syntax highlight
(cond ((fboundp 'global-font-lock-mode)
      (global-font-lock-mode t)
;; Maximum colors
      (setq font-lock-maximum-decoration t)))

;; iswitchb is fastest (i missed something? :)
(iswitchb-mode 1)

;; window configuration undo/redo is really useful
(winner-mode 1)

;; end of general
;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; keybindings

(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-,") 'bs-show)
(global-set-key (kbd "C-.") 'iswitchb-buffer)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-w") 'kill-region-or-word)
(global-set-key (kbd "C-x C-r") 'query-replace-regexp)
(global-set-key (kbd "C-x C-a") 'imenu)
(global-set-key (kbd "M-<up>") 'prh:move-line-up)
(global-set-key (kbd "M-<down>") 'prh:move-line-down)
(global-set-key (kbd "C-M-<up>") 'prh:duplicate-line-up)
(global-set-key (kbd "C-M-<down>") 'prh:duplicate-line-down)

;; add some nifty things
(load "dired-x")
(global-unset-key (kbd "C-x C-j"))
(global-set-key (kbd "C-x C-d") 'dired-jump)

(global-set-key (kbd "<f5>") 'kmacro-end-and-call-macro)

(global-set-key (kbd "C-c C-k")  (lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "C-M-l") (lambda () (interactive) (switch-to-buffer (other-buffer))))
(global-set-key (kbd "C-M-z") (lambda (arg char) (interactive "p\ncZap backward to char: ") (zap-to-char (- arg) char)))

(when win32
    (global-set-key (kbd "C-<f12>") '(lambda () (interactive) (w32-send-sys-command 61488 nil)))
  )

;; end of keybindings
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; bs-show

(add-hook 'bs-mode-hook 'no-scroll-margin)

(setq bs-configurations
      '(("files" nil nil nil files-without-org bs-sort-buffer-interns-are-last)
        ("all" nil nil nil nil nil)
        ("dired" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (eq major-mode 'dired-mode)))) nil)
        ("org" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (eq major-mode 'org-mode)))) nil)
        ("circe" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (memq major-mode '(circe-channel-mode circe-server-mode))))) nil)))

(setq bs-default-configuration "files")
(setq bs-alternative-configuration "all")

(defun files-without-org (buffer)
  "Return true when buffer is file and not org-mode"
  (or
   (not (buffer-file-name buffer))
   (with-current-buffer buffer (eq major-mode 'org-mode))))

;; bs-show end
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Frame setup

(server-start)

(if graf
    (progn
      ;; Title formatting
      (setq frame-title-format (list "emacs - "  '(buffer-file-name "%f" "%b")))
      (setq icon-title-format frame-title-format)

      ;; Font setup
      (if win32
          (add-to-list 'default-frame-alist '(font . "-outline-Unifont-normal-r-normal-normal-16-120-96-96-c-*-*"))
        (add-to-list 'default-frame-alist '(font . "-*-terminus-*-*-*-*-16-*-*-*-*-*-iso10646-1")))

      ;; bar setup
      (menu-bar-mode 0)
      (tool-bar-mode 0)
      (when linux
        (scroll-bar-mode -1)))
  (menu-bar-mode 0)
)

(when win32
  (defvar safe-language-change-flag nil)
  (defun safe-language-change ()
    (interactive)
    (setq safe-language-change-flag (not safe-language-change-flag))
    (when safe-language-change-flag
      (toggle-input-method)
      (w32-toggle-lock-key 'capslock)))
  (global-set-key (kbd "<language-change>") 'safe-language-change)
)

;; end frame setup
;;;;;;;;;;;;;;;;;;

;;;;;;;;;
;; python

(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-hook 'python-mode-hook
          (lambda ()
            (set beginning-of-defun-function 'py-beginning-of-def-or-class)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (eldoc-mode 1)
            (setq show-trailing-whitespace t)
            ))

(when
    (require 'pymacs nil t)
  (pymacs-load "ropemacs" "rope-")
)

;; end of python
;;;;;;;;;;;;;;;;

;;;;;;;;;
;; erlang

(autoload 'erlang-mode "erlang-mode.el"
  "Major mode for editing Erlang source files" t)

(add-hook 'erlang-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)
            (setq show-trailing-whitespace t)
            ))

;; end of erlang
;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; various modes

(autoload 'css-mode "css-mode.el" "Major mode for editing CSS files" t)
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(autoload 'wikipedia-mode "wikipedia-mode.el" "Major mode for editing MediaWiki files" t)
(autoload 'factor-mode "factor.el" "factor" t)

(setq hooks-with-traling
      '(
        css-mode-hook
        emacs-lisp-mode-hook
        factor-mode-hook
        wikipedia-mode-hook
        markdown-mode-hook
        erlang-mode-hook
        haskell-mode-hook
        ))

(dolist (hook hooks-with-traling)
  (add-hook hook '(lambda () (setq show-trailing-whitespace t))))

(setq hooks-wants-filladapt
      '(
        markdown-mode-hook
        wikipedia-mode-hook
        ))

(dolist (hook hooks-wants-filladapt)
  (add-hook hook '(lambda () (filladapt-mode t))))

(setq auto-mode-alist
      (append
       (list
        '("\\.md$" . markdown-mode)
        '("\\.css$" . css-mode)
        '("\\.erl$" . erlang-mode)
        '("\\.hs$" . haskell-mode)
        '("\\.wiki\\.txt$" . wikipedia-mode)
        '("\\.factor" . factor-mode)
        )
        auto-mode-alist))

(setq factor-binary "~/bin/factor")
(setq factor-image "~/var/factor/factor.image")

(setq w3m-use-cookies t)

(autoload 'window-number-mode "window-number" nil t)
(autoload 'window-number-control-mode "window-number" nil t)
(window-number-mode 1)
(window-number-control-mode 1)

;; end of modes
;;;;;;;;;;;;;;;;;;

;;;;;;;;;
;; eshell

(setq
 eshell-cmpl-cycle-completions nil
 eshell-buffer-shorthand t
 eshell-output-filter-functions '(eshell-handle-control-codes eshell-watch-for-password-prompt eshell-postoutput-scroll-to-bottom)
 eshell-scroll-show-maximum-output t
 eshell-scroll-to-bottom-on-output nil
 eshell-scroll-to-bottom-on-input 'this
)

;; scroll to bottom for eshell

(defun eshell-scroll-to-bottom (window display-start)
  (if (and window (window-live-p window))
      (let ((resize-mini-windows nil))
	(save-selected-window
	  (select-window window)
	  (save-restriction
	    (widen)
	    (when (> (point) eshell-last-output-start) ; we're editing a line. Scroll.
	      (save-excursion
		(recenter -1)
		(sit-for 0))))))))

(defun eshell-add-scroll-to-bottom ()
  (interactive)
  (make-local-hook 'window-scroll-functions)
  (add-hook 'window-scroll-functions 'eshell-scroll-to-bottom nil t))

(add-hook 'eshell-mode-hook 'eshell-add-scroll-to-bottom)

(defun eshell/e (&rest args)
  (if (null args)
      (bury-buffer)
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defsubst eshell/ls (&rest args)
  "An alias version of `eshell-do-ls'."
  (let ((insert-func 'eshell-buffered-print)
        (error-func 'eshell-error)
        (flush-func 'eshell-flush)
        (args-plus (append
                    (cond ((not (eq (car (aref eshell-current-handles 1)) t))
                           (list "-1")))
                    args)))
    (eshell-do-ls args-plus)))

(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(add-hook 'eshell-mode-hook '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))

;; end of eshell
;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; Functions

(defalias 'qrr 'query-replace-regexp)

(defun no-scroll-margin ()
  "Set scroll-margin to 0 buffer-locally"
  (interactive)
  (set (make-local-variable 'scroll-margin) 0))

(defun autocompile ()
  "compile itself if ~/.emacs"
  (interactive)
  (if (string= (buffer-file-name) (concat default-directory ".emacs"))
      (byte-compile-file (buffer-file-name))))

(defun insert-date (format)
  "Wrapper around format-time-string."
  (interactive "MFormat: ")
  (insert (format-time-string format)))

(defun insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%c")))

(defun whole-line ()
  "Returns list of two values - beginning of this line
and beginning of next line, for deleting/copying"
  (list (line-beginning-position) (line-beginning-position 2)))

(defun prh:copy-line ()
  "Save current line into Kill-Ring without marking the line "
  (buffer-substring (line-beginning-position) (line-end-position))
  )

(defun prh:check-newline (line)
  "Checks that line ends in newline. Adds it if not."
  (if (eql (aref line (1- (length line))) ?\n)
      line
    (concat line "\n")
  ))

(defun prh:cut-line ()
  "Kills current line"
  (setq prh:cutted-line (apply 'buffer-substring (whole-line)))
  (apply 'delete-region (whole-line))
  (prh:check-newline prh:cutted-line)
  )

(defun prh:count-lines (arg)
  "Count lines depending on arg.
If arg is positive, count from current position to end,
if negative, count from start to current position.
"
  (if (> arg 0)
      (count-lines (point) (point-max))
   (count-lines (point-min) (point)) 1))

(defun prh:move-line (&optional arg)
  "Move current line.
Arg determines number of lines to skip, negative means move up."
  (interactive "p")
  (if (> (prh:count-lines arg) 0)
      (let ((prh:column (current-column)))
        (progn
          (or arg (setq arg 1))
          (forward-line 1)
          (transpose-lines arg)
          (forward-line -1)
          (move-to-column prh:column)))
    ))

(defun prh:move-line-down (&optional arg)
  "Move current line down. Optional ARG determines number of lines to skip"
  (interactive "p")
  (or arg (setq arg 1))
  (prh:move-line arg)
)

(defun prh:move-line-up (&optional arg)
  "Move current line up. Optional ARG determines number of lines to skip"
  (interactive "p")
  (or arg (setq arg 1))
  (prh:move-line (- arg))
)

(defun prh:duplicate-line (&optional arg)
  "Copy current line.
Arg determines number of lines to be created and direction."
  (interactive "p")
  (let ((prh:column (current-column)))
    (progn
      (or arg (setq arg 1))
      (if (< arg 0)
          (setq tomove (1+ arg))
        (setq tomove arg))
      (setq prh:cutted-line (prh:copy-line))
      (end-of-line tomove)
      (newline)
      (insert prh:cutted-line)
      (next-line (- arg))
      (move-to-column prh:column)))
  )

(defun prh:duplicate-line-down (&optional arg)
  "Duplicate current line down. Optional ARG determines number of lines to skip"
  (interactive "p")
  (or arg (setq arg 1))
  (prh:duplicate-line arg)
)

(defun prh:duplicate-line-up (&optional arg)
  "Duplicate current line up. Optional ARG determines number of lines to skip"
  (interactive "p")
  (or arg (setq arg 1))
  (prh:duplicate-line (- arg))
)

(defun kill-region-or-word (&optional arg)
  "If region is active, kill it, backward kill word in other case."
  (interactive "p")
  (if (and transient-mark-mode mark-active)
      (kill-region (point) (mark))
    (backward-kill-word arg)
    ))

;; end of functions
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; Theme

(when graf
  (require 'color-theme)
  (color-theme-initialize)
  (load-file "~/.el/pastels-on-dark-theme.el")
  (color-theme-pastels-on-dark)
)

;; end
;;;;;;

;;;;;;;;;
;; Jabber
(when
    (require 'jabber nil t)
  (setq
   jabber-nickname "piranha"
   jabber-resource "el"
   jabber-server "eth0.net.ua"
   jabber-username "piranha")

  (setq jabber-roster-line-format " %-25n %u %-8s  %S")
  (setq jabber-history-enabled t)
  (setq jabber-use-global-history nil)
  (setq jabber-history-dir "~/.emacs.d/jabber/")

  (add-hook 'jabber-chat-mode-hook
            (lambda ()
              (setq fill-column 120)
              (no-scroll-margin)
              ))

  (define-key jabber-chat-mode-map [escape] 'my-jabber-chat-bury)

  (defun my-jabber-chat-bury ()
    (interactive)
    (if (eq 'jabber-chat-mode major-mode)
        (bury-buffer)))

  (add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)
  (setq jabber-autoaway-method 'jabber-xprintidle-program)

  ;; stub for announce
  (setq jabber-xosd-display-time 3)

  (defun jabber-xosd-display-message (message)
    "Displays MESSAGE through the xosd"
    (let ((process-connection-type nil))
      (start-process "jabber-xosd" nil "osd_cat" "-p" "bottom" "-A" "center" "-f" "-*-courier-*-*-*-*-30" "-d" (number-to-string jabber-xosd-display-time))
      (process-send-string "jabber-xosd" message)
      (process-send-eof "jabber-xosd")))

  (defun jabber-message-xosd (from buffer text proposed-alert)
    (jabber-xosd-display-message "New message"))

  (add-to-list 'jabber-alert-message-hooks
               'jabber-message-xosd)
)
;; end of jabber
;;;;;;;;;;;;;;;;

;;;;;;
;; irc

(autoload 'circe "circe" "" t)

(when (file-directory-p "~/.el/circe")
  (add-to-list 'load-path "~/.el/circe"))

(when (file-exists-p "~/.secrets.el")
  (load-file "~/.secrets.el"))

(setq
 circe-default-nick "_piranha_"
 circe-default-user "piranha"
 circe-default-realname "Alexander Solovyov"
 circe-ignore-list nil
 circe-server-auto-join-channels
 '(("^freenode$" "#concatenative" "#conkeror"))
 circe-nickserv-passwords
 `(("freenode" ,freenode-password)))

(eval-after-load "circe"
  '(progn
     (require 'lui-irc-colors)
     (add-to-list 'lui-pre-output-hook 'lui-irc-colors)
     (require 'circe-log)
     (enable-circe-log)
     (add-hook 'lui-mode-hook
               (lambda ()
                 (set (make-local-variable 'scroll-conservatively) 8192)
                 (local-set-key (kbd "C-,") (lambda ()
                                              (interactive)
                                              (bs--show-with-configuration "circe")))
                 (no-scroll-margin))
     )))

(defun irc ()
  "Connect to IRC."
  (interactive)
  (circe "irc.freenode.net" "6667" "freenode"))

;; end of irc
;;;;;;;;;;;;;

;;;;;;;;;;;
;; org-mode

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-log-done t
      org-return-follows-link t
      org-hide-emphasis-markers t
      org-hide-leading-stars t
      org-odd-levels-only t)

(add-hook 'org-mode-hook '(lambda () (interactive)
                            (local-set-key (kbd "C-,") (lambda ()
                                                         (interactive)
                                                         (bs--show-with-configuration "org")))
                            ))


;; end of org-mode
;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/org/life.org" "~/org/mydeco.org" "~/org/musicx.org")))
 '(safe-local-variable-values (quote ((prompt-to-byte-compile) (encoding . utf-8)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
