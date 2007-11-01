;; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU Emacs config
;; by Alexander Solovyov
;; piranha AT piranha DOT org DOT ua
;;
;; Special thank to all, who help me in creation, especially to:
;; Yuriy Sazonets <haze AT astral.ntu-kpi.kiev.ua>
;; Alex Ott <ottalex AT narod.ru>
;; Emacswiki.org ;)
;;
;; $Id: .emacs 33 2007-09-16 16:11:58Z piranha $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (set-keyboard-coding-system 'utf-8))
(when win32
  (set-clipboard-coding-system 'cp1251-dos)
  (set-selection-coding-system 'cp1251-dos)
  (set-default-coding-systems 'cp1251-dos)
  (set-keyboard-coding-system 'cp1251-dos)
  (set-w32-system-coding-system 'cp1251-dos))

(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;
;; Extensions
;;;;;;;;;;;;;

;; loadpath
(setq load-path (cons (expand-file-name "~/.el") load-path))
(when (file-exists-p "/usr/share/emacs/site-lisp/site-gentoo.el")
  (load "/usr/share/emacs/site-lisp/site-gentoo"))

(require 'gnus-load nil t)
(require 'filladapt nil t)
(require 'session nil t)
(require 'htmlize nil t)
(require 'django-html-mode nil t)
(require 'psvn nil t)
(require 'grep+ nil t)

;; Use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

;; add some nifty things
(load "dired-x")

;;;;;;;;;;
;; General

;; filladapt
(setq-default filladapt-mode t)

;; don't ask when changing case
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)

;; overwrite mode is not disabled
(put 'overwrite-mode 'disabled nil)

;; don't ask, just do it!
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

;; end of general
;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; keybindings

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-/") 'dabbrev-completion)
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x C-r") 'query-replace-regexp)
(global-set-key (kbd "C-x C-a") 'imenu)

(global-set-key (kbd "<f5>") 'call-last-kbd-macro)

(global-set-key (kbd "C-x w")  (lambda () (interactive) (kill-buffer nil)))
(global-set-key '[(control meta l)] (lambda () (interactive) (switch-to-buffer (other-buffer))))

(when win32
    (global-set-key [C-f12] '(lambda () (interactive) (w32-send-sys-command 61488 nil)))
  )

;; end of keybindings
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Frame setup

(server-start)

(if graf
    (progn
      ;; Title formatting
      (setq frame-title-format (list '(buffer-file-name "%b aka %f") " - GNU Emacs " emacs-version "@" (downcase system-name)))
      (setq icon-title-format frame-title-format)

      ;; Font setup
      (if win32
          (add-to-list 'default-frame-alist '(font . "-outline-Unifont-normal-r-normal-normal-16-120-96-96-c-*-*"))
        (add-to-list 'default-frame-alist '(font . "-*-terminus-*-*-*-*-16-*-*-*-*-*-*-*")))
      ;; Default Frame
      (add-to-list 'default-frame-alist '(fullscreen . fullwidth))

      ;; bar setup
      (menu-bar-mode 1)
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

(global-set-key [C-S-iso-lefttab] 'tabbar-backward-tab)
(global-set-key [C-S-tab] 'tabbar-backward-tab)
(global-set-key [C-tab] 'tabbar-forward-tab)
(global-set-key [C-f10] 'tabbar-local-mode)

(set-face-foreground 'tabbar-default "Gray")
(set-face-background 'tabbar-default "Gray15")
(set-face-foreground 'tabbar-selected "pale green")
(set-face-bold-p 'tabbar-selected t)
(set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "gray72"))

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
          ((find (aref (buffer-name (current-buffer)) 0) " *") "*")
          (t "All Buffers"))
         )))

(tabbar-mode 1)

;; tabbar end
;;;;;;;;;;;;;

;;;;;;;;;
;; python

(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-hook 'python-mode-hook
          (lambda ()
            (set beginning-of-defun-function 'py-beginning-of-def-or-class)
            (local-set-key [return] 'newline-and-indent)
            (eldoc-mode 1)
            (setq show-trailing-whitespace t)
            ))

;; end of python
;;;;;;;;;;;;;;;;

;;;;;;;;;
;; erlang

(autoload 'erlang-mode "erlang-mode.el"
  "Major mode for editing Erlang source files" t)

(add-hook 'erlang-mode-hook
          (lambda ()
            (local-set-key [return] 'newline-and-indent)
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

(add-hook 'markdown-mode-hook
          (lambda ()
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
    (local-set-key "\C-tab" 'indent-for-tab-command)))

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
              (local-set-key [tab] 'dabbrev-expand)
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
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
