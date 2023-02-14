(put 'test-case-name 'safe-local-variable '(lambda (x) t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes '("0261c7e47f570afd984db039e083ec0f89eb4194" default))
 '(js2-basic-offset 2)
 '(js2-mode-indent-ignore-first-tab t)
 '(magit-diff-arguments '("--ignore-space-change" "--no-ext-diff" "--stat"))
 '(mini-frame-show-parameters '((top . 20) (width . 0.7) (left . 0.4) (height . 10)))
 '(ns-alternate-modifier '(:ordinary meta :function meta :mouse alt))
 '(org-agenda-files '("~/dev/work/timing.org"))
 '(package-selected-packages
   '(clj-refactor cider magit-section vterm corfu-prescient vertico-prescient consult corfu vertico tree-sitter undo-tree tsx-mode typescript-mode benchmark-init swift-mode jinja2-mode clojure-mode pyvenv embark lua-mode yasnippet-snippets point-stack flycheck-clj-kondo zig-mode relative-buffers imenu-anywhere multiple-cursors sass-mode deft ialign po-mode restclient vcl-mode mini-frame ctrlf web-mode deadgrep lastfm flimenu terraform-mode zen-mode gnu-elpa-keyring-update writeroom-mode iflipb string-edit hl-todo clj-refactor-mode projectile fzf minions org-bullets flycheck-joker expand-region circe iedit graphviz-dot-mode string-inflection sql-indent whole-line-or-region highlight-parentheses flycheck-pyflakes flycheck-pos-tip flycheck less-css-mode yaml-mode go-mode markdown-mode dockerfile-mode magit dumb-jump toml-mode inflections))
 '(safe-local-variable-values
   '((whitespace-style face tabs trailing lines-tail)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook 'write-contents-functions
                     (lambda nil
                       (delete-trailing-whitespace)
                       nil))
           (require 'whitespace)
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t)
     (js-indent-level . 2)
     (encoding . utf-8)
     (prompt-to-byte-compile)))
 '(semantic-default-submodes
   '(global-semantic-idle-scheduler-mode global-semanticdb-minor-mode))
 '(send-mail-function 'sendmail-send-it)
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
