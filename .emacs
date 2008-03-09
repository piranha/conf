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
(require 'grep+ nil t)
(require 'mercurial nil t)
(require 'etags nil t)
(setq tags-file-name (expand-file-name "~/TAGS"))

;; Use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

;;;;;;;;;;
;; General

(defmacro fun-for-bind (func &rest args)
  "Returns a symbol of an anonymous interactive function,
suitable for binding to keys."
  `(lambda () (interactive) (,func ,@args)))

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
;; But don't complain if it's not shown
(setq mark-even-if-inactive t)

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

(global-set-key (kbd "C-c k") (fun-for-bind kill-buffer nil))
(global-set-key (kbd "C-M-l") (fun-for-bind switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-M-z") (lambda (&optional arg char) (interactive "p\ncZap backward to char: ") (zap-to-char (- arg) char)))
(global-set-key (kbd "C-M-y") (lambda (&optional arg) (interactive "*p") (yank-pop (- arg))))

(when win32
    (global-set-key (kbd "C-<f12>") (fun-for-bind w32-send-sys-command 61488 nil))
  )

;; end of keybindings
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; bs-show

(eval-after-load "bs" '(define-key bs-mode-map "z" 'bs-switch-to-files-and-refresh))
(add-hook 'bs-mode-hook 'no-scroll-margin)

(setq bs-configurations
      '(("files" nil nil nil files-without-org bs-sort-buffer-interns-are-last)
        ("org" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (memq major-mode '(org-mode))))) nil)
        ("jabber" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (memq major-mode '(jabber-chat-mode jabber-roster-mode))))) nil)
        ("circe" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (memq major-mode '(circe-channel-mode circe-server-mode circe-query-mode))))) nil)
        ("dired" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (memq major-mode '(dired-mode))))) nil)
        ("all" nil nil nil nil nil)))

(setq bs-default-configuration "files")
(setq bs-alternative-configuration "all")

(defun files-without-org (buffer)
  "Return true when buffer is file and not org-mode"
  (or
   (not (buffer-file-name buffer))
   (with-current-buffer buffer (eq major-mode 'org-mode))))

(defun bs-switch-to-files-and-refresh ()
  "Apply \"files\" configuration. Refresh whole Buffer Selection Menu."
  (interactive)
  (bs-set-configuration "files")
  (setq bs-default-configuration bs-current-configuration)
  (bs--redisplay t)
  (bs--set-window-height))


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

(autoload 'python-mode "python" "Python editing mode." t)

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "RET") 'newline-and-indent)
     (when (require 'pymacs nil t) (pymacs-load "ropemacs" "rope-"))
     (defun rope-reload ()
       (interactive)
       (pymacs-terminate-services)
       (pymacs-load "ropemacs" "rope-"))
     ))


;; end of python
;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; various modes

(autoload 'erlang-mode "erlang-mode" "Erlang edit mode" t)
(autoload 'css-mode "css-mode" "Major mode for editing CSS files" t)
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(autoload 'wikipedia-mode "wikipedia-mode" "Major mode for editing MediaWiki files" t)
(autoload 'factor-mode "factor" "factor" t)
(autoload 'django-html-mode "django-html-mode" "Django HTML templates" t)

(eval-after-load "erlang-mode" '(define-key erlang-mode-map (kbd "RET") 'newline-and-indent))

(setq hooks-with-trailing
      '(emacs-lisp-mode-hook
        factor-mode-hook
        wikipedia-mode-hook
        markdown-mode-hook
        erlang-mode-hook
        haskell-mode-hook
        python-mode-hook
        ))

(dolist (hook hooks-with-trailing) (add-hook hook 'display-trailing-whitespace))

(setq hooks-wants-filladapt
      '(markdown-mode-hook
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

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

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

(eval-after-load "eshell" '(define-key eshell-mode-map (kbd "C-a") 'eshell-maybe-bol))

;; end of eshell
;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; Functions

(defalias 'qrr 'query-replace-regexp)

(defun no-scroll-margin ()
  "Set scroll-margin to 0 buffer-locally"
  (interactive)
  (set (make-local-variable 'scroll-margin) 0))

(defun display-trailing-whitespace ()
  "Enable display of trailing whitespaces buffer-locally"
  (interactive)
  (set (make-local-variable 'show-trailing-whitespace) t))

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
          (setq prh:cutted-line (prh:cut-line))
          (setq prh:cutted-line (prh:check-newline prh:cutted-line))
          (forward-line arg)
          (insert prh:cutted-line)
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

(defun toggle-file (desired-file)
  "Toggle buffer display of desired file."
  (when (file-exists-p desired-file)
    (if (and (buffer-file-name)
             (string= (expand-file-name desired-file)
                      (expand-file-name (buffer-file-name))))
        (bury-buffer)
      (find-file desired-file))))

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

(when (file-directory-p "~/.el/jabber")
  (add-to-list 'load-path "~/.el/jabber"))
(autoload 'jabber-connect "jabber" "Emacs Jabber client" t)
(global-set-key (kbd "C-x C-j C-c") 'jabber-connect)

(setq
 jabber-account-list '(("asolovyov@mydeco.com"
                        (:network-server . "chat.mydeco.com")
                        (:port . 5223)
                        (:connection-type . ssl))
                       ("piranha@eth0.net.ua"))
 jabber-muc-autojoin '("dreamteam@conference.mydeco.com")
 jabber-roster-line-format " %-25n %u %-8s  %S"
 jabber-history-enabled t
 jabber-use-global-history nil
 jabber-history-dir "~/.emacs.d/jabber/"
 jabber-chat-buffer-format "chat-%n"
 jabber-groupchat-buffer-format "muc-%n"
 )

(eval-after-load "jabber"
  '(progn
     (add-hook 'jabber-chat-mode-hook 'no-scroll-margin)

     (define-key jabber-chat-mode-map (kbd "C-,") (fun-for-bind bs--show-with-configuration "jabber"))
     (define-key jabber-roster-mode-map (kbd "C-,") (fun-for-bind bs--show-with-configuration "jabber"))

     (add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)
     (setq jabber-autoaway-method 'jabber-xprintidle-program)

     (when linux
       (setq jabber-notify-display-time 3)
       (setq jabber-notify-max-length 30)

       (defun jabber-notify-display (title text)
         "Displays MESSAGE with TITLE through the libnotify"
         (let (
               (process-connection-type nil)
               (length (* 1000 jabber-notify-display-time))
               (message (if (> (length text) jabber-notify-max-length)
                            (concat (substring text 0 jabber-notify-max-length) "...")
                          text))
               )
           (start-process "jabber-notify" nil "notify-send" "-t" (number-to-string length) title message)
           (process-send-eof "jabber-notify")))

       (defun jabber-message-notify (from buffer text proposed-alert)
         (let ((title (car (split-string from "@"))))
           (jabber-notify-display title text)))

       (defun jabber-muc-notify (nick group buffer text proposed-alert)
         (let ((group-name (car (split-string group "@"))))
           (let ((title (concat nick "@" group-name)))
             (jabber-notify-display title text))))

       (defun jabber-scroll-to-bottom (window)
         "Scroll the input line to the bottom of the window."
         (when (and window
                    (window-live-p window))
           (let ((resize-mini-windows nil))
             ;; This is to prevent an XEmacs byte compilation warning
             ;; "variable bound but not referred to". XEmacs is trying to be
             ;; too intelligent.
             (when (featurep 'xemacs)
               (declare (special resize-mini-windows)))
             (save-selected-window
               (select-window window)
               (save-restriction
                 (widen)
                 (when (>= (point) jabber-point-insert)
                   (save-excursion
                     (goto-char (point-max))
                     (recenter -1)
                     (sit-for 0))))))))

       (defun jabber-muc-stb (nick group buffer text proposed-alert)
         (dolist (window (get-buffer-window-list buffer))
           (jabber-scroll-to-bottom window)))

       (defun jabber-message-stb (from buffer text proposed-alert)
         (dolist (window (get-buffer-window-list buffer))
           (jabber-scroll-to-bottom window)))

       (define-personal-jabber-alert jabber-muc-notify)

       (setq jabber-alert-message-hooks '(jabber-message-scroll
                                          jabber-message-notify
                                          jabber-message-stb))
       (setq jabber-alert-muc-hooks '(jabber-muc-scroll
                                      jabber-muc-notify-personal
                                      jabber-muc-stb))
       (setq jabber-alert-presence-hooks '())
       (setq jabber-alert-info-message-hooks '(jabber-info-display))
       )
     ))

;; end of jabber
;;;;;;;;;;;;;;;;

;;;;;;
;; irc

(autoload 'circe "circe" "Sane IRC client" t)

(when (file-directory-p "~/.el/circe")
  (add-to-list 'load-path "~/.el/circe"))

(when (file-exists-p "~/.secrets.el")
  (load-file "~/.secrets.el"))

(setq
 circe-default-nick "piranha"
 circe-default-user "piranha"
 circe-default-realname "Alexander Solovyov"
 circe-ignore-list nil
 circe-server-killed-confirmation 'ask-and-kill-all
 circe-server-auto-join-channels
 '(("^freenode$" "#concatenative" "#conkeror" "#emacs" "#org-mode"))
 circe-nickserv-passwords
 `(("freenode" ,freenode-password)))

(eval-after-load "circe"
  '(progn
     (require 'lui-irc-colors)
     (add-to-list 'lui-pre-output-hook 'lui-irc-colors)
     (require 'lui-logging)
     (setq lui-logging-directory "~/.emacs.d/circe")
     (enable-lui-logging)
     (require 'circe-nickcolor)
     (enable-circe-nickcolor)
     (define-key lui-mode-map (kbd "C-,") (fun-for-bind bs--show-with-configuration "circe"))
     (add-hook 'lui-mode-hook 'no-scroll-margin)))

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

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-,") (fun-for-bind bs--show-with-configuration "org"))))

(global-set-key (kbd "<f1>") (fun-for-bind toggle-file "~/org/life.org"))
(global-set-key (kbd "<f2>") (fun-for-bind toggle-file "~/org/musicx.org"))
(global-set-key (kbd "<f3>") (fun-for-bind toggle-file "~/org/mydeco.org"))

;; end of org-mode
;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/org/life.org" "~/org/mydeco.org" "~/org/musicx.org"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )