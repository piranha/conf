(server-start)

(when prh:graf
  ;; Title formatting
  (setq frame-title-format (list "emacs - "  '(buffer-file-name "%f" "%b")))
  (setq icon-title-format frame-title-format)

  ;; Font setup
  ;; nix font is configured in resources
  (if prh:win
      (add-to-list 'default-frame-alist '(font . "-outline-Unifont-normal-r-normal-normal-16-120-96-96-c-*-*")))

  (tool-bar-mode 0)
  (scroll-bar-mode -1))

(if (or (not (eq system-type 'darwin))
        (not prh:graf))
    (menu-bar-mode 0)
  (menu-bar-mode 1))

;; switch emacs lang by windows' system key (capslock in my case)
(when prh:win
  (defvar safe-language-change-flag nil)
  (defun safe-language-change ()
    (interactive)
    (setq safe-language-change-flag (not safe-language-change-flag))
    (when safe-language-change-flag
      (toggle-input-method)
      (w32-toggle-lock-key 'capslock)))
  (global-set-key (kbd "<language-change>") 'safe-language-change))

;; syntax highlight
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t)
  (setq font-lock-maximum-decoration t))

(deftheme better-tango
  "Tango with white background and some colors close to gtk-ide color-theme.")
(custom-theme-set-faces
 'better-tango
 '(cursor ((((class color) (min-colors 89)) (:background "#204a87"))))
 '(fringe ((((class color) (min-colors 89)) (:background "#d3d7cf"))))
 '(highlight ((((class color) (min-colors 89)) (:background "#babdb6"))))
 '(region ((((class color) (min-colors 89)) (:background "#babdb6"))))
 '(secondary-selection ((((class color) (min-colors 89)) (:background "#8cc4ff"))))
 '(isearch ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#ce5c00"))))
 '(lazy-highlight ((((class color) (min-colors 89)) (:background "#e9b96e"))))
 '(trailing-whitespace ((((class color) (min-colors 89)) (:background "#ef2929"))))
 ;; '(mode-line ((((class color) (min-colors 89)) (:box (:line-width -1 :style released-button) :background "#d3d7cf" :foreground "#2e3436"))))
 ;; '(mode-line-inactive ((((class color) (min-colors 89)) (:box (:line-width -1 :style released-button) :background "#888a85" :foreground "#2e3436"))))
 ;; '(minibuffer-prompt ((((class color) (min-colors 89)) (:weight bold :foreground "#204a87"))))
 '(escape-glyph ((((class color) (min-colors 89)) (:foreground "#a40000"))))
 '(error ((((class color) (min-colors 89)) (:foreground "#a40000"))))
 '(warning ((((class color) (min-colors 89)) (:foreground "#ce5c00"))))
 '(success ((((class color) (min-colors 89)) (:foreground "#4e9a06"))))
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "#75507b"))))
 '(font-lock-comment-face ((((class color) (min-colors 89)) (:slant italic :foreground "#5f615c"))))
 '(font-lock-constant-face ((((class color) (min-colors 89)) (:weight bold :foreground "#204a87"))))
 '(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "#5c3566"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "#2e3436"))))
 '(font-lock-keyword-face ((((class color) (min-colors 89)) (:foreground "#346604"))))
 '(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "#a40000"))))
 '(font-lock-type-face ((((class color) (min-colors 89)) (:foreground "#204a87"))))
 '(link ((((class color) (min-colors 89)) (:underline t :foreground "#204a87"))))
 '(link-visited ((((class color) (min-colors 89)) (:underline t :foreground "#3465a4"))))
 '(smerge-refined-change ((((class color) (min-colors 89)) (:background "#ad7fa8"))))
 '(ediff-current-diff-A ((((class color) (min-colors 89)) (:background "#729fcf"))))
 '(ediff-fine-diff-A ((((class color) (min-colors 89)) (:background "#ad7fa8"))))
 '(ediff-current-diff-B ((((class color) (min-colors 89)) (:background "#fce94f"))))
 '(ediff-fine-diff-B ((((class color) (min-colors 89)) (:background "#fcaf3e"))))
 '(flyspell-duplicate ((((class color) (min-colors 89)) (:underline "#fcaf3e"))))
 '(flyspell-incorrect ((((class color) (min-colors 89)) (:underline "#ef2929"))))
 '(semantic-decoration-on-includes ((((class color) (min-colors 89)) (:underline "#346604"))))
 '(semantic-decoration-on-private-members-face ((((class color) (min-colors 89)) (:background "#d3d7cf"))))
 '(semantic-decoration-on-protected-members-face ((((class color) (min-colors 89)) (:background "#d3d7cf"))))
 '(semantic-decoration-on-unknown-includes ((((class color) (min-colors 89)) (:background "#8f5902"))))
 '(semantic-decoration-on-unparsed-includes ((((class color) (min-colors 89)) (:underline "#ce5c00"))))
 '(semantic-tag-boundary-face ((((class color) (min-colors 89)) (:overline "#729fcf"))))
 '(semantic-unmatched-syntax-face ((((class color) (min-colors 89)) (:underline "#ef2929"))))
 '(default ((t (:background "#ffffff" :foreground "#2e3436")))))

(when prh:graf
  (load-theme 'better-tango t))
