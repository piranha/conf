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
    (set-face-attribute 'mode-line-inactive nil :background "#dfdfdf")
    (set-face-attribute 'font-lock-string-face nil :foreground "#3a7821")
    (set-face-attribute 'font-lock-comment-face nil :foreground "#a40000")))

(when (eq window-system 'x)
  (set-frame-font "Monaco-12" nil t)
  (set-face-attribute 'default t :font "Monaco-12"))

(when (or (eq window-system 'ns)
          (eq window-system 'mac))
  (setq ns-use-native-fullscreen nil)
  (setq ns-use-fullscreen-animation nil)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta))
