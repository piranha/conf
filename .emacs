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
;; $Id: .emacs 9 2006-09-20 05:54:11Z piranha $
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

;;;;;;;;;;
;; lang setup & changing with system switcher
;;;;;;;;;;

(require 'mule)
(require 'codepage)

(codepage-setup 866)
(codepage-setup 1251)
(setq default-input-method "cyrillic-jcuken-ms")
(define-coding-system-alias 'windows-1251 'cp1251)
(define-coding-system-alias 'koi8-ru 'koi8-u)

(when linux
  (set-clipboard-coding-system 'koi8-u)
  (set-selection-coding-system 'koi8-u)
  (set-default-coding-systems 'koi8-u)
  (set-keyboard-coding-system 'koi8-u))
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

(require 'color-theme)
;(require 'gnus-load)
(require 'psvn)
(require 'filladapt)
(require 'session)
(require 'prh-bufsw)
(require 'htmlize)
;(eval-after-load "buff-menu" '(require 'buff-menu-plus)) 

;;;;;;;;;;
;; General

;; bar setup
(if win32
    ((menu-bar-mode 1)
     (tool-bar-mode 0))
  (menu-bar-mode 0))
;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; filladapt
(setq-default filladapt-mode t)

;; don't ask when changing case
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)

;; overwrite mode is not disabled
(put 'overwrite-mode 'disabled nil)

(setq
  ;; don't show annoing startup msg
  inhibit-startup-message t
  ;; NO annoing backups
  make-backup-files nil
  ;; see what you type
  echo-keystrokes 0.1
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
  ;; next-line don't add new lines 
  next-line-add-newlines nil
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
  ;; where i am
  ;calendar-latitude 51.05
  ;calendar-longitude -2.39
  ;calendar-location-name "Ieper"
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
  dired-recursive-copies t
  dired-recursive-deletes t
)

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

;; Use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

;; highlight marked text
(transient-mark-mode t)

; to highlight ( and )
;(show-paren-mode t)

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

;; moving between buffers
(setq stesla-hated-buffer-regexps '("^ " "*Buffer" "^\\*trace" "^\\*tramp" "^\\*"))
(pc-bufsw::bind-keys [C-tab] [C-S-tab])
(pc-bufsw::bind-keys [f12] [f11])
(setq pc-bufsw::quite-time 2)

;; undo
(global-set-key (kbd "C-z") 'undo)

;; completion
(global-set-key (kbd "C-/") 'dabbrev-completion)

;; go to line
(global-set-key (kbd "C-M-g") 'goto-line)

;; C-a - mark all buffer
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; C-(home|end) in linux console
(global-set-key "[7^" 'beginning-of-buffer)
(global-set-key "[8^" 'end-of-buffer)


;; kill current buffer
(defun prh:kill-current-buffer ()
	(interactive)
	(kill-buffer (current-buffer)))

(global-set-key (kbd "C-x w") 'prh:kill-current-buffer)

;; end of keybindings
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Frame setup

(when linux
  (server-start))

(when win32
  ;; GNUserv
  (require 'gnuserv)
  (gnuserv-start)

  ;; Font setup
  (set-default-font "-outline-Unifont-normal-r-normal-normal-16-120-96-96-c-*-*")
  (setq w32-enable-synthesized-fonts nil)
  
  ;; size & position
  (set-frame-height (selected-frame) 56)
  (set-frame-width (selected-frame) 154)
  (set-frame-position (selected-frame) 0 0)
  
  ;; Title formatting
  (setq frame-title-format (list '(buffer-file-name "%f" "%b") " - GNU Emacs " emacs-version "@" system-name ))
  (setq icon-title-format frame-title-format)
  
  (when (eq window-system 'w32)
    (defun restore-frame (&optional frame)
      "Restore FRAME to previous size (default: current frame)."
      (interactive)
      (w32-send-sys-command 61728 frame)))
  
  (when (eq window-system 'w32)
    (defun maximize-frame (&optional frame)
      "Maximize FRAME (default: current frame)."
      (interactive)
      (w32-send-sys-command 61488 frame)))
  
  (when (eq window-system 'w32)
    (defalias 'minimize-frame (if (fboundp 'really-iconify-frame)
                                  'really-iconify-frame
                                'iconify-frame)))
  
  (defun prh:ajust-frame ()
    "Ajusts current frame to display properties"
    (interactive)
    (set-default-font "-outline-Unifont-normal-r-normal-normal-16-120-96-96-c-*-*")
    (w32-send-sys-command 61488))
  
  (global-set-key [C-f12] 'prh:ajust-frame)
  
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

;;;;;;;;;
;; python

(add-hook 'python-mode-hook
		  (lambda ()
            (set (make-variable-buffer-local 'beginning-of-defun-function)
                 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")
            (local-set-key [return] 'reindent-then-newline-and-indent)
			(turn-on-auto-fill)
			(eldoc-mode 1)
			(define-key python-mode-map "\"" 'electric-pair)
			(define-key python-mode-map "\'" 'electric-pair)
			(define-key python-mode-map "(" 'electric-pair)
			(define-key python-mode-map "[" 'electric-pair)
			(define-key python-mode-map "{" 'electric-pair)))

;(autoload 'py-complete-init "py-complete")
;(add-hook 'python-mode-hook 'py-complete-init) 

;; end of python
;;;;;;;;;;;;;;;;

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
        '("\\.xml" . docbook-mode))
       auto-mode-alist))

;; end of Docbook settings
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; Functions

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

(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

;; end of functions
;;;;;;;;;;;;;;;;;;;

;;;;;;;;
;; Custom hooks
(add-hook 'after-save-hook 'autocompile())

(add-hook 'after-init-hook 'session-initialize)

;; end of hooks
;;;;;;;;;;;;;;;

(when win32
  (color-theme-initialize)
  (color-theme-subtle-hacker))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(canlock-password "a8a433cb25fe5cc792f85b4081f1f02f08030bf7"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
