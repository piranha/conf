(server-start)

;; syntax highlight
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t)
  (setq font-lock-maximum-decoration t))

(when (not (eq window-system 'nil))
  ;; Title formatting
  (setq frame-title-format (list "emacs - "  '(buffer-file-name "%f" "%b")))
  (setq icon-title-format frame-title-format)

  (tool-bar-mode 0)
  (menu-bar-mode 1)
  (scroll-bar-mode -1)
  (load-theme 'better-tango t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#dfdfdf")))

;; switch emacs lang by windows' system key (capslock in my case)
(when (eq system-type 'windows-nt)
  (defvar safe-language-change-flag nil)
  (defun safe-language-change ()
    (interactive)
    (setq safe-language-change-flag (not safe-language-change-flag))
    (when safe-language-change-flag
      (toggle-input-method)
      (w32-toggle-lock-key 'capslock)))
  (global-set-key (kbd "<language-change>") 'safe-language-change))

(when (eq window-system 'x)
  (set-frame-font "Monaco-12" nil t)
  (set-face-attribute 'default t :font "Monaco-12"))

(when (eq window-system 'mac)
  (setq ns-use-native-fullscreen nil))