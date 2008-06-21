(server-start)

(when graf
  ;; Title formatting
  (setq frame-title-format (list "emacs - "  '(buffer-file-name "%f" "%b")))
  (setq icon-title-format frame-title-format)

  ;; Font setup
  ;; nix font is configured in resources
  (if win
      (add-to-list 'default-frame-alist '(font . "-outline-Unifont-normal-r-normal-normal-16-120-96-96-c-*-*")))

  (tool-bar-mode 0)
  (scroll-bar-mode -1)
)

(menu-bar-mode 0)

;; switch emacs lang by windows' system key (capslock in my case)
(when win
  (defvar safe-language-change-flag nil)
  (defun safe-language-change ()
    (interactive)
    (setq safe-language-change-flag (not safe-language-change-flag))
    (when safe-language-change-flag
      (toggle-input-method)
      (w32-toggle-lock-key 'capslock)))
  (global-set-key (kbd "<language-change>") 'safe-language-change))

;; syntax highlight
(cond ((fboundp 'global-font-lock-mode)
      (global-font-lock-mode t)
;; Maximum colors
      (setq font-lock-maximum-decoration t)))

(when win
  (require 'color-theme)
  (color-theme-initialize)
  (load-file "~/.emacs.d/packages/themes/pastels-on-dark-theme.el")
;  (color-theme-charcoal-black)
  (color-theme-pastels-on-dark)
)
