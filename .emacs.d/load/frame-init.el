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

  (tool-bar-mode 0)
  (menu-bar-mode 1)
  (scroll-bar-mode -1)
  (load-theme 'better-tango t)
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 2 :color "grey75"))
  (set-face-attribute 'mode-line-inactive nil
                      :box '(:line-width 2 :color "grey90"))
  (set-face-attribute 'font-lock-string-face nil :foreground "#3A7821")
  (set-face-attribute 'font-lock-comment-face nil :foreground "#A40000")
  (set-face-attribute 'font-lock-doc-face nil :foreground "#007DA6")
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))

(when (eq window-system 'x)
  (set-frame-font "Monaco-12" nil t)
  (set-face-attribute 'default t :font "Monaco-12"))

(when (or (eq window-system 'ns)
          (eq window-system 'mac))
  (setq ns-use-native-fullscreen nil
        ns-use-fullscreen-animation nil
        mac-command-modifier 'meta
        mac-option-modifier 'meta))
