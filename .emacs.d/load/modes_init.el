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

(setq
 dired-bind-jump nil
 dired-omit-extensions '(".pyc"))
(autoload 'dired-jump "dired-x" "Jump to dir of current file" t)
(autoload 'dired-omit-mode "dired-x" "Omit unnecessary files in dired view" t)
(add-hook 'dired-mode-hook 'dired-omit-mode)
(eval-after-load "dired"
  '(define-key dired-mode-map (kbd "C-,") (fun-for-bind bs--show-with-configuration "dired")))

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
;(iswitchb-mode 1)

;; IDO, I need to learn your power
(ido-mode 1)
(setq
 ido-enable-flex-matching 1
 ido-show-dot-for-dired t
 ido-auto-merge-work-directories-length -1 ; disable auto-merging
 ido-confirm-unique-completion t)

(winner-mode 1) ;; window configuration undo/redo

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; major modes

(autoload 'erlang-mode "erlang" "Erlang edit mode" t)
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(autoload 'wikipedia-mode "wikipedia-mode" "Major mode for editing MediaWiki files" t)
(autoload 'factor-mode "factor" "factor" t)
(autoload 'django-html-mode "django-html-mode" "Django HTML templates" t)
(autoload 'haskell-mode "haskell-mode" "Major mode for editing Haskell sources" t)

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
        django-html-mode-hook
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
        '("\\.html\\'" . django-html-mode)
        )
        auto-mode-alist))

(setq factor-binary "~/bin/factor")
(setq factor-image "~/var/factor/factor.image")
(eval-after-load "factor"
  '(progn
     (define-key factor-mode-map (kbd "C-M-l") (fun-for-bind switch-to-buffer (other-buffer)))
     (define-key factor-listener-mode-map (kbd "C-M-l") (fun-for-bind switch-to-buffer (other-buffer)))))

(setq w3m-use-cookies t)

(when nix
  (setq browse-url-browser-function 'browse-url-firefox
        browse-url-new-window-flag  t
        browse-url-firefox-new-window-is-tab t))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;
;; Python
;;;;;;;;;

;(autoload 'python-mode "python" "Python editing mode." t)

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "RET") 'newline-and-indent)
     (when (require 'pymacs nil t) (pymacs-load "ropemacs" "rope-"))
     (define-key ropemacs-local-keymap (kbd "M-/") 'dabbrev-expand)
     (defun rope-reload ()
       (interactive)
       (pymacs-terminate-services)
       (pymacs-load "ropemacs" "rope-"))
     ))


;; Snippets

(require 'yasnippet nil t)
(eval-after-load "yasnippet"
'(progn
   (setq
    yas/trigger-key (kbd "C-/")
    yas/next-field-key (kbd "C-/"))
   (yas/initialize)
   (yas/load-directory "~/.emacs.d/snippets/")))
