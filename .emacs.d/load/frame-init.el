(server-start)

;; syntax highlight
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t)
  (setq font-lock-maximum-decoration t))

(when (not (eq window-system 'nil))
  ;; Title formatting
  (setq frame-title-format (list "emacs - "  '(buffer-file-name "%f" "%b")))
  (setq icon-title-format frame-title-format)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))

  (setq fast-but-imprecise-scrolling t)
  (setq jit-lock-defer-time 0)

  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1)
  ;;(load-theme 'better-tango t)
  (load-theme 'alabaster t)
  (set-face-attribute 'font-lock-string-face nil :background "#FFF"))

(when (eq window-system 'x)
  (set-frame-font "Monaco-12" nil t)
  (set-face-attribute 'default t :family "Monaco" :height 120))

(when (or (eq window-system 'ns)
          (eq window-system 'mac))
  (set-fontset-font t 'symbol "Apple Color Emoji")
  (setq ns-use-native-fullscreen nil
        ns-use-fullscreen-animation nil
        mac-command-modifier 'meta
        mac-option-modifier 'meta))
