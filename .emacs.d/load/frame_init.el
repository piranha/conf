(server-start)

(when graf
  ;; Title formatting
  (setq frame-title-format (list "emacs - "  '(buffer-file-name "%f" "%b")))
  (setq icon-title-format frame-title-format)

  ;; Font setup
  (if win
      (add-to-list 'default-frame-alist '(font . "-outline-Unifont-normal-r-normal-normal-16-120-96-96-c-*-*"))
    (add-to-list 'default-frame-alist '(font . "-*-terminus-*-*-*-*-16-*-*-*-*-*-iso10646-1")))

  (tool-bar-mode 0)
  (when nix
    (scroll-bar-mode -1))
)

(menu-bar-mode 0)

(when win
  (defvar safe-language-change-flag nil)
  (defun safe-language-change ()
    (interactive)
    (setq safe-language-change-flag (not safe-language-change-flag))
    (when safe-language-change-flag
      (toggle-input-method)
      (w32-toggle-lock-key 'capslock)))
  (global-set-key (kbd "<language-change>") 'safe-language-change)
)

;; syntax highlight
(cond ((fboundp 'global-font-lock-mode)
      (global-font-lock-mode t)
;; Maximum colors
      (setq font-lock-maximum-decoration t)))

(when graf
  (require 'color-theme)
  (color-theme-initialize)
  (load-file "~/.emacs.d/packages/themes/pastels-on-dark-theme.el")
  (color-theme-pastels-on-dark))
