;; various small modes configuration

;; experiment (require 'gnus-load nil t)

(autoload 'gnus "gnus" "Best email client ever" t)

;; Use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

(autoload 'filladapt-mode "filladapt" "Minor mode to adaptively set fill-prefix and overload filling functions" t)
(autoload 'htmlize-buffer "htmlize" "Convert buffer text and decorations to HTML" t)
(autoload 'grep "grep+" "Extensions to standard library `grep.el'" t)

(autoload 'find-tag "etags" "etags facility" t)
(setq tags-file-name (expand-file-name "~/TAGS"))

(autoload 'session-initialize "session" "Use variables, registers and buffer places across sessions" t)
(add-hook 'after-init-hook 'session-initialize)

(setq dired-bind-jump nil)
(autoload 'dired-jump "dired-x" "Jump to dir of current file" t)

(display-time)

(column-number-mode 1)

;; I hate blinking
(if (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; It's rare, but annoying
(if (fboundp 'global-semantic-stickyfunc-mode)
    (global-semantic-stickyfunc-mode -1))

;; highlight marked text
(transient-mark-mode 1)
;; but work even without it
(setq mark-even-if-inactive t)

(show-paren-mode 1)

;; iswitchb is fastest (i missed something? :)
(iswitchb-mode 1)

;; window configuration undo/redo is really useful
(winner-mode 1)

(autoload 'window-number-mode "window-number" nil t)
(autoload 'window-number-control-mode "window-number" nil t)
(window-number-mode 1)
(window-number-control-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; major modes

(autoload 'erlang-mode "erlang" "Erlang edit mode" t)
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(autoload 'wikipedia-mode "wikipedia-mode" "Major mode for editing MediaWiki files" t)
(autoload 'factor-mode "factor" "factor" t)
(autoload 'django-html-mode "django-html-mode" "Django HTML templates" t)

(eval-after-load "erlang-mode"
  '(define-key erlang-mode-map (kbd "RET") 'newline-and-indent))

(setq hooks-with-trailing
      '(emacs-lisp-mode-hook
        factor-mode-hook
        wikipedia-mode-hook
        markdown-mode-hook
        erlang-mode-hook
        haskell-mode-hook
        python-mode-hook
        ))

(dolist (hook hooks-with-trailing) (add-hook hook 'display-trailing-whitespace))

(setq hooks-wants-filladapt
      '(markdown-mode-hook
        wikipedia-mode-hook
        ))

(dolist (hook hooks-wants-filladapt)
  (add-hook hook '(lambda () (filladapt-mode t))))

(setq auto-mode-alist
      (append
       (list
        '("\\.md\\'" . markdown-mode)
        '("\\.erl\\'" . erlang-mode)
        '("\\.hs\\'" . haskell-mode)
        '("\\.wiki\\.txt\\'" . wikipedia-mode)
        '("\\.factor\\'" . factor-mode)
        )
        auto-mode-alist))

(setq factor-binary "~/bin/factor")
(setq factor-image "~/var/factor/factor.image")

(setq w3m-use-cookies t)

(add-hook 'isearch-mode-end-hook
          (lambda ()
            ;; it works a little better if isearch puts you
            ;; at the start of the search, not the end
            (when isearch-forward (goto-char isearch-other-end))
            ))

(setq browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag  t
      browse-url-firefox-new-window-is-tab t)

;;;;;;;;;
;; Python
;;;;;;;;;

(autoload 'python-mode "python" "Python editing mode." t)

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "RET") 'newline-and-indent)
     (when (require 'pymacs nil t) (pymacs-load "ropemacs" "rope-"))
     (defun rope-reload ()
       (interactive)
       (pymacs-terminate-services)
       (pymacs-load "ropemacs" "rope-"))
     ))
