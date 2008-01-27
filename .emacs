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

(require 'mule)
(require 'codepage)

(codepage-setup 866)
(codepage-setup 1251)
(setq default-input-method "russian-computer")
(define-coding-system-alias 'windows-1251 'cp1251)
(define-coding-system-alias 'koi8-ru 'koi8-u)

(when linux
  (set-selection-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq
   x-select-enable-clipboard t
   x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
   interprogram-paste-function (quote x-cut-buffer-or-selection-value)
   ))

(when win32
  (set-clipboard-coding-system 'cp1251-dos)
  (set-selection-coding-system 'cp1251-dos)
  (set-default-coding-systems 'cp1251-dos)
  (set-keyboard-coding-system 'cp1251-dos)
  (set-w32-system-coding-system 'cp1251-dos))

(prefer-coding-system 'utf-8-unix)

;;;;;;;;;;;;;
;; Extensions
;;;;;;;;;;;;;

;; loadpath
(add-to-list 'load-path (expand-file-name "~/.el"))
(when (file-exists-p "/usr/share/emacs/site-lisp/site-gentoo.el")
  (load "/usr/share/emacs/site-lisp/site-gentoo"))

(require 'gnus-load nil t)
(require 'filladapt nil t)
(require 'session nil t)
(require 'htmlize nil t)
(require 'django-html-mode nil t)
(require 'psvn nil t)
(require 'grep+ nil t)
(require 'mercurial nil t)

(require 'etags nil t)
(setq tags-file-name (expand-file-name "~/TAGS"))

;; Use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

;;;;;;;;;;
;; General

;; filladapt
(setq-default filladapt-mode t)

;; don't ask, just do it!
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'overwrite-mode 'disabled nil)
(put 'set-goal-column 'disabled nil)

(setq
  ;; don't show annoing startup msg
  inhibit-startup-message t
  ;; NO annoing backups
  make-backup-files nil
  ;; follow symlinks and don't ask
  vc-follow-symlinks t
  ;; see what you type
  echo-keystrokes 0.01
  ;; text scrolling
  scroll-conservatively 50
  scroll-preserve-screen-position 't
  scroll-margin 10
  ;; Insert space/slash after completion
  comint-completion-addsuffix t
  ;; number of chars in line
  fill-column 72
  ;; delete line in one stage
  kill-whole-line t
  ;; default mode
  default-major-mode 'text-mode
  ;; meaning are the same as the name :)
  delete-key-deletes-forward t
  ;; Scroll by one line at a time
  scroll-step 1
  ;; don't add new lines when scrolling down
  next-line-add-newlines nil
  ;; make sure file ends with NEWLINE
  require-final-newline t
  ;; delete excess backup versions
  delete-old-versions t
  ;; setting the default tabulation
  default-tab-width 4
  ;; paste at cursor NOT at mouse pointer position
  mouse-yank-at-point t
  ;; apropos works better but slower
  apropos-do-all t
  ;; display time in the modeline
  display-time-24hr-format t
  display-time-day-and-date t
  ;; calendar customizing
  european-calendar-style t
  calendar-week-start-day 1
  ;; autosave every 512 keyboard inputs
  auto-save-interval 512
  ;; limit the number of newest versions
  kept-new-versions 5
  ;; limit the number of oldest versions
  kept-old-versions 5
  auto-save-list-file-prefix "~/.emacs.d/backups/save-"
  ;; don't beep
;  visible-bell t
  cursor-in-non-selected-windows nil
  ;; dired settings
  dired-recursive-copies 'top
  dired-recursive-deletes 'top
  ;; safe variables to set in buffer
  safe-local-variable-values '((encoding . utf-8))
)

;; display time
(display-time)

;; save position in files
(setq-default save-place t)

;; no blinking cursor
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

;; case INsensitive search
(setq-default case-fold-search t)

;; do not use tabs for indentation
(setq-default indent-tabs-mode nil)

;; This tells emacs to show the column number in each modeline.
(column-number-mode 1)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight marked text
(transient-mark-mode 1)

;; to highlight ( and )
(show-paren-mode 1)

;; Turn off this idiotic stickyfunc mode
(if (boundp 'global-semantic-stickyfunc-mode)
    (global-semantic-stickyfunc-mode -1))

;; syntax highlight
(cond ((fboundp 'global-font-lock-mode)
;; Turn on font-lock in all modes that support it
      (global-font-lock-mode t)
;; Maximum colors
      (setq font-lock-maximum-decoration t)))

;; ido is nice thing
(ido-mode 1)

;; end of general
;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; keybindings

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x w") 'kill-region)
(global-set-key (kbd "C-x C-r") 'query-replace-regexp)
(global-set-key (kbd "C-x C-a") 'imenu)
(global-set-key (kbd "M-<up>") 'prh:move-line-up)
(global-set-key (kbd "M-<down>") 'prh:move-line-down)
(global-set-key (kbd "C-c l") 'copy-line)
(global-set-key (kbd "C-M-<up>") 'prh:duplicate-line-up)
(global-set-key (kbd "C-M-<down>") 'prh:duplicate-line-down)

;; add some nifty things
(load "dired-x")
(global-unset-key (kbd "C-x C-j"))
(global-set-key (kbd "C-x C-d") 'dired-jump)

(global-set-key (kbd "<f5>") 'kmacro-end-and-call-macro)

(global-set-key (kbd "C-x C-k")  (lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "C-M-l") (lambda () (interactive) (switch-to-buffer (other-buffer))))
(global-set-key (kbd "C-M-z") (lambda (char) (interactive "cZap backward to char: ") (zap-to-char -1 char)))

(when win32
    (global-set-key (kbd "C-<f12>") '(lambda () (interactive) (w32-send-sys-command 61488 nil)))
  )

;; end of keybindings
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Frame setup

(server-start)

(if graf
    (progn
      ;; Title formatting
      (setq frame-title-format (list "%b"  '(buffer-file-name " aka %f") " - Emacs " emacs-version))
      (setq icon-title-format frame-title-format)

      ;; Font setup
      (if win32
          (add-to-list 'default-frame-alist '(font . "-outline-Unifont-normal-r-normal-normal-16-120-96-96-c-*-*"))
        (add-to-list 'default-frame-alist '(font . "-*-terminus-*-*-*-*-16-*-*-*-*-*-iso10646-1")))
        ;(add-to-list 'default-frame-alist '(font . "-*-andale mono-*-*-*-*-15-*-*-*-*-*-iso10646-1")))

      ;; Default Frame
      (add-to-list 'default-frame-alist '(fullscreen . fullscreen))

      ;; bar setup
      (menu-bar-mode 0)
      (tool-bar-mode 0)
      (when linux
        (scroll-bar-mode -1)))
  (menu-bar-mode 0)
)

(when win32
  (defadvice server-find-file (before server-find-file-in-one-frame activate)
    "Make sure that the selected frame is stored in `gnuserv-frame', and raised."
    (setq gnuserv-frame (selected-frame))
    (raise-frame))
  (defadvice server-edit (before server-edit-in-one-frame activate)
    "Make sure that the selected frame is stored in `gnuserv-frame', and lowered."
    (setq gnuserv-frame (selected-frame))
    (lower-frame))
)

;; end frame setup
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; ibuffer

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; default groups for ibuffer
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("python" (or
                          (name . "^\\*Python\\*$")
                          (mode . django-html-mode)
                          (mode . python-mode)))
               ("jabber" (or
                          (name . "^\\*-jabber-")))
               ("erc" (or
                       (mode . erc-mode)))
               ("erlang" (or
                          (name . "^\\*erlang\\*$")
                          (mode . erlang-mode)))
               ("haskell" (or
                          (mode . haskell-mode)))
               ("dired" (or
                          (mode . dired-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\.emacs$")
                         (name . "^\\.gnus$")
                         (name . "^\\*Messages\\*$")))
               ("gnus" (or
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble")))))))

(setq ibuffer-formats
      '((mark modified read-only " "
             (name 28 28 :left :elide)
             " "
             (size 9 -1 :right)
             " "
             (mode 16 16 :left :elide)
             " " filename-and-process)
       (mark " "
             (name 16 -1)
             " " filename)))


;; ibuffer, I like my buffers to be grouped
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups
             "default")))

;; end of ibuffer
;;;;;;;;;;;;;;;;;

;;;;;;;;;
;; Tabbar

(require 'tabbar)

(global-set-key (kbd "C-S-<iso-lefttab>") 'tabbar-backward-tab)
(global-set-key (kbd "C-S-<tab>") 'tabbar-backward-tab)
(global-set-key (kbd "C-<tab>") 'tabbar-forward-tab)
(global-set-key (kbd "C-<f10>") 'tabbar-local-mode)

(set-face-foreground 'tabbar-default "gray")
(set-face-background 'tabbar-default "black")
(set-face-foreground 'tabbar-selected "pale green")
(set-face-bold-p 'tabbar-selected t)
(set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "gray72"))
(when linux
  (set-face-font 'tabbar-default "-*-terminus-*-*-*-*-16-*-*-*-*-*-iso10646-1"))

(setq tabbar-buffer-groups-function
      (lambda ()
        (list
         (cond
          ((string-match "^\\*-jabber-" (buffer-name (current-buffer))) "jabber")
          ((eq major-mode 'erc-mode) "ERC")
          ((eq major-mode 'dired-mode) "Dired")
          ((or
            (eq major-mode 'message-mode)
            (eq major-mode 'bbdb-mode)
            (eq major-mode 'mail-mode)
            (eq major-mode 'gnus-group-mode)
            (eq major-mode 'gnus-summary-mode)
            (eq major-mode 'gnus-article-mode)
            (string-match "^\\.\\(bbdb\\|newsrc-dribble\\)" (buffer-name (current-buffer))))
           "gnus")
          ((eq major-mode 'tags-table-mode) "*")
          ((find (aref (buffer-name (current-buffer)) 0) " *") "*")
          (t "All Buffers"))
         )))

(tabbar-mode 1)

;; tabbar end
;;;;;;;;;;;;;

;;;;;;;;;
;; eshell

(setq
 eshell-cmpl-cycle-completions nil
 eshell-buffer-shorthand t
 eshell-output-filter-functions '(eshell-handle-control-codes eshell-watch-for-password-prompt eshell-postoutput-scroll-to-bottom)
 eshell-scroll-show-maximum-output t
 eshell-scroll-to-bottom-on-output nil
)

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

(add-hook 'eshell-mode-hook
          '(lambda ()
             (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)
             ))

(defun eshell/deb (&rest args)
  (eshell-eval-using-options
   "deb" args
   '((?f "find" t find "list available packages matching a pattern")
     (?i "installed" t installed "list installed debs matching a pattern")
     (?l "list-files" t list-files "list files of a package")
     (?s "show" t show "show an available package")
     (?v "version" t version "show the version of an installed package")
     (?w "where" t where "find the package containing the given file")
     (nil "help" nil nil "show this usage information")
     :show-usage)
   (eshell-do-eval
    (eshell-parse-command
     (cond
      (find
       (format "apt-cache search %s" find))
      (installed
       (format "dlocate -l %s | grep '^.i'" installed))
      (list-files
       (format "dlocate -L %s | sort" list-files))
      (show
       (format "apt-cache show %s" show))
      (version
       (format "dlocate -s %s | egrep '^(Package|Status|Version):'" version))
      (where
       (format "dlocate %s" where))))
    t)))

;; end of eshell
;;;;;;;;;;;;;;;;

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

(autoload 'css-mode "css-mode.el"
  "Major mode for editing CSS files" t)

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(autoload 'wikipedia-mode "wikipedia-mode.el"
  "Major mode for editing MediaWiki files" t)

(autoload 'lout-mode "lout-mode.el"
  "Major mode for editing Lout files" t)
(autoload 'factor-mode "factor.el"
  "factor" t)

(add-hook 'css-mode-hook
          (lambda () (setq show-trailing-whitespace t)))

(add-hook 'html-mode-hook
          (lambda () (setq show-trailing-whitespace t)))

(add-hook 'latex-mode-hook
          (lambda ()
            (local-set-key (kbd "\"") 'self-insert-command)
            (setq fill-column 80)
            (auto-fill-mode)
            ))

(setq auto-mode-alist
      (append
       (list
        '("\\.md$" . markdown-mode)
        '("\\.css$" . css-mode)
        '("\\.erl$" . erlang-mode)
        '("\\.hs$" . haskell-mode)
        '("\\.wiki\\.txt$" . wikipedia-mode)
        '("\\.lout$" . lout-mode)
        '("\\.factor" . factor-mode)
        )
        auto-mode-alist))

(setq w3m-use-cookies t)

;; end of modes
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; DTD mode settings

(autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
(autoload 'dtd-etags "tdtd"
  "Execute etags on FILESPEC and match on DTD-specific regular expressions."
  t)
(autoload 'dtd-grep "tdtd" "Grep for PATTERN in files matching FILESPEC." t)

;; Turn on font lock when in DTD mode
(add-hook 'dtd-mode-hooks
          'turn-on-font-lock)

(setq auto-mode-alist
      (append
       (list
    '("\\.dcl$" . dtd-mode)
    '("\\.dec$" . dtd-mode)
    '("\\.dtd$" . dtd-mode)
    '("\\.ele$" . dtd-mode)
    '("\\.ent$" . dtd-mode)
    '("\\.mod$" . dtd-mode))
       auto-mode-alist))

;; To use resize-minibuffer-mode, uncomment this and include in your .emacs:
;;(resize-minibuffer-mode)

;; end of DTD mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;; DocBook settings

(autoload 'docbook-mode "docbookide" "Major mode for DocBook documents." t)
;(add-hook 'docbook-mode-hook 'docbook-menu-mode)
(add-hook
 'docbook-mode-hook
 '(lambda ()
    (local-set-key (kbd "C-<tab>") 'indent-for-tab-command)))

;; You might want to make this the default for .sgml or .xml documents,
;; or you might want to rely on -*- DocBook -*- on the first line,
;; or perhaps buffer variables. It's up to you...
(setq auto-mode-alist
      (append
       (list
        '("\\.sgm" . docbook-mode)
        '("\\.sgml" . docbook-mode)
        '("\\.dbk" . docbook-mode))
       auto-mode-alist))

;; end of Docbook settings
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; Functions

;; alias for qrr
(defalias 'qrr 'query-replace-regexp)

;; Autocompilation
(defun autocompile()
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

(defun copy-line ()
  "Save current line into Kill-Ring without mark the line "
  (interactive)
  (let ((beg (line-beginning-position))
      	(end (line-end-position)))
    (copy-region-as-kill beg end))
  )

(defun prh:kill-line ()
  "Kills current line"
  (interactive)
  (beginning-of-line)
  (kill-line 1)
  )

(defun prh:count-lines (arg)
  "Count lines depending on arg.
If arg is positive, count from current position to end,
if negative, count from start to current position.
"
  (if (> arg 0)
      (count-lines (point) (point-max))
    (- (count-lines 1 (point)) 1)))

(defun prh:move-line (&optional arg)
  "Move current line.
Arg determines number of lines to skip, negative means move up."
  (interactive "p")
  (if (> (prh:count-lines arg) 0)
      (let ((prh:column (current-column)))
        (progn
          (or arg (setq arg 1))
          (prh:kill-line)
          (next-line arg)
          (yank)
          (next-line -1)
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
      (copy-line)
      (end-of-line tomove)
      (newline)
      (yank)
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

;; end of functions
;;;;;;;;;;;;;;;;;;;

;;;;;;;;
;; Custom hooks
;(add-hook 'after-save-hook 'autocompile())

(add-hook 'after-init-hook 'session-initialize)

;; end of hooks
;;;;;;;;;;;;;;;

;;;;;;;;;;
;; Finesse

(when graf
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-charcoal-black)
)

;; end
;;;;;;

;;;;;;;;;
;; Jabber
(when
    (require 'jabber nil t)
  (setq
   jabber-nickname "piranha"
   jabber-resource "laptop"
   jabber-server "eth0.net.ua"
   jabber-username "piranha")

  (setq jabber-roster-line-format " %c %-25n %u %-8s  %S\n")
  (setq jabber-history-enabled t)
  (setq jabber-use-global-history nil)
  (setq jabber-history-dir "~/.emacs.d/jabber/")

  (add-hook 'jabber-chat-mode-hook
            (lambda ()
              (setq fill-column 120)
              (local-set-key (kbd "<tab>") 'dabbrev-expand)
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

  (defun jabber-message-xosd (from buffer text propsed-alert)
    (jabber-xosd-display-message "New message"))

  (add-to-list 'jabber-alert-message-hooks
               'jabber-message-xosd)
)
;; end of jabber
;;;;;;;;;;;;;;;;

;;;;;;
;; ERC

(setq
 erc-nick "piranha"
 erc-user-full-name "Alexander Solovyov"
 erc-server "irc.freenode.net"
 erc-auto-query 'buffer
 )

(setq erc-modules
      '(autoaway
        autojoin
        button
        completion
        fill
        irccontrols
        log
        match
        menu
        netsplit
        noncommands
        readonly
        ring
        scrolltobottom
        stamp
        track))

;; end of erc
;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((prompt-to-byte-compile) (encoding . utf-8)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
