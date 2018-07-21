(put 'test-case-name 'safe-local-variable '(lambda (x) t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("0261c7e47f570afd984db039e083ec0f89eb4194" default)))
 '(js2-basic-offset 4)
 '(js2-mode-indent-ignore-first-tab t)
 '(mac-option-modifier (quote (:ordinary meta :function meta :mouse alt)))
 '(magit-diff-arguments (quote ("--ignore-space-change" "--no-ext-diff" "--stat")))
 '(org-agenda-files (quote ("~/dev/work/timing.org")))
 '(package-selected-packages
   (quote
    (hl-todo clj-refactor clj-refactor-mode cider projectile ivy-prescient fzf ivy-posframe moody minions vcl-mode ialign org-bullets flycheck-joker git-timemachine lua-mode expand-region rainbow-delimiters circe bbdb counsel iedit imenu-anywhere swiper restclient graphviz-dot-mode string-inflection sql-indent whole-line-or-region ag highlight-parentheses flycheck-pyflakes flycheck-pos-tip flycheck po-mode less-css-mode sass-mode yaml-mode go-mode markdown-mode dockerfile-mode magit dumb-jump use-package utop toml-mode let-alist inflections)))
 '(safe-local-variable-values
   (quote
    ((whitespace-style face tabs trailing lines-tail)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t)
     (js-indent-level . 2)
     (encoding . utf-8)
     (prompt-to-byte-compile))))
 '(semantic-default-submodes
   (quote
    (global-semantic-idle-scheduler-mode global-semanticdb-minor-mode)))
 '(send-mail-function (quote sendmail-send-it))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-handled-backends nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#ffffff" :foreground "#2e3436" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Monaco"))))
 '(fixed-pitch ((t (:foreground "dark green"))))
 '(org-column ((t (:strike-through nil :underline nil :weight normal :height 120 :family "outline-unifont"))))
 '(org-hide ((t (:foreground "grey77")))))
