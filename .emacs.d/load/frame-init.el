;; -*- lexical-binding: t; -*-
(server-start)

;; syntax highlight
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t)
  (setq font-lock-maximum-decoration t))

(defun dark-mode-enabled-p ()
  "If dark mode is enabled right now?"
  ;; (string=
  ;;  (ns-do-applescript "tell application \"System Events\" to tell appearance preferences to return \"\" & dark mode")
  ;;  "true")
  (string= ns-system-appearance "dark"))

;;; https://github.com/LionyxML/auto-dark-emacs/
(defun set-gui-theme! (&rest _args)
  (when (not (eq window-system 'nil))
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
    ;; (if (dark-mode-enabled-p)
    ;;     (load-theme 'alabaster-dark t)
    ;;   (load-theme 'alabaster t))
    (if (dark-mode-enabled-p)
        (load-theme 'mccarthy-dark t)
      (load-theme 'mccarthy t))))

;; this is from `brew install emacs-plus`
(add-hook 'ns-system-appearance-change-functions #'set-gui-theme!)

(when (not (eq window-system 'nil))
  ;; Title formatting
  (setq frame-title-format (list "emacs - "  '(buffer-file-name "%f" "%b")))
  (setq icon-title-format frame-title-format)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))

  (setq jit-lock-defer-time 0)
  (pixel-scroll-precision-mode 1)

  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1)
  (set-gui-theme!)
  (tooltip-mode -1)
  ;; this disables showing docs on mouse hover, I hate it
  (setq show-help-function nil)
  ;; this fixes code in markdown being displayed with weird font
  (set-face-attribute 'fixed-pitch nil :family 'unspecified))


(when (eq window-system 'x)
  (set-frame-font "Monaco-12" nil t)
  (set-face-attribute 'default nil :family "Monaco" :height 120))

(when (or (eq window-system 'ns)
          (eq window-system 'mac))
  (set-frame-font "Monaco-12" nil t)
  (set-fontset-font t 'symbol "Apple Color Emoji")
  (set-fontset-font t nil "SF Pro Display" nil 'append)
  (setq ns-use-native-fullscreen nil
        ns-use-fullscreen-animation nil
        mac-command-modifier 'meta
        mac-option-modifier 'meta))

(setq-default mode-line-format
              '(" "
                mode-line-front-space
                mode-line-client
                mode-line-frame-identification
                mode-line-buffer-identification
                mode-line-modified
                " " mode-line-position
                (multiple-cursors-mode mc/mode-line)
                " " mode-line-modes
                " " mode-line-misc-info
                mode-line-end-spaces))

(use-package minions
  :ensure t
  :init (minions-mode 1))

(use-package moody
  :ensure t
  :config

  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line nil          :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line nil          :box nil)
    (set-face-attribute 'mode-line-inactive nil :box nil)
    (set-face-attribute 'mode-line-inactive nil :background "#bbb" :foreground "#000"))

  (setq x-underline-at-descent-line nil)
  (setq global-mode-string (remove 'display-time-string global-mode-string))
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification))

;;; frame-init.el ends here
