;; various small modes configuration

;(autoload 'gnus "gnus" "Best email client ever" t)

(autoload 'filladapt-mode "filladapt" "Minor mode to adaptively set fill-prefix and overload filling functions" t)
(autoload 'htmlize-buffer "htmlize" "Convert buffer text and decorations to HTML" t)
(autoload 'grep "grep+" "Extensions to standard library `grep.el'" t)

(autoload 'find-tag "etags" "etags facility" t)
(setq tags-file-name (expand-file-name "~/TAGS"))

(autoload 'session-initialize "session" "Use variables, registers and buffer places across sessions" t)
(add-hook 'after-init-hook 'session-initialize)

;; dired
(setq
 dired-bind-jump nil
 dired-omit-extensions '(".pyc" ".elc"))
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

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(autoload 'wikipedia-mode "wikipedia-mode" "Major mode for editing MediaWiki files" t)
(autoload 'factor-mode "factor" "factor" t)
(autoload 'django-html-mode "django-html-mode" "Django HTML templates" t)
(autoload 'haskell-mode "haskell-mode" "Major mode for editing Haskell sources" t)
(autoload 'io-mode "io-mode" "Major mode for editing Io sources" t)
(autoload 'js2-mode "js2" nil t)

(setq hooks-with-trailing
      '(emacs-lisp-mode-hook
        factor-mode-hook
        wikipedia-mode-hook
        markdown-mode-hook
        erlang-mode-hook
        haskell-mode-hook
        python-mode-hook
        django-html-mode-hook))
(dolist (hook hooks-with-trailing) (add-hook hook 'display-trailing-whitespace))

(setq hooks-want-fill
      '(markdown-mode-hook
        wikipedia-mode-hook))
(dolist (hook hooks-want-fill)
  (add-hook hook '(lambda () (filladapt-mode t)))
  (add-hook hook 'turn-on-auto-fill))

(setq auto-mode-alist
      (append
       (list
        '("\\.md\\'" . markdown-mode)
        '("\\.erl\\'" . erlang-mode)
        '("\\.hs\\'" . haskell-mode)
        '("\\.wiki\\.txt\\'" . wikipedia-mode)
        '("\\.factor\\'" . factor-mode)
        '("\\.html\\'" . django-html-mode)
        '("\\.egg\\'" . archive-mode)
        '("\\.io\\'" . io-mode)
        )
        auto-mode-alist))

(setq factor-binary "~/bin/factor")
(setq factor-image "~/var/factor/factor.image")
(eval-after-load "factor"
  '(progn
     (define-key factor-mode-map (kbd "C-M-l")
       (fun-for-bind switch-to-buffer (other-buffer)))
     (define-key factor-listener-mode-map (kbd "C-M-l")
       (fun-for-bind switch-to-buffer (other-buffer)))))

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

;; Erlang

(when (file-directory-p "~/var/distel/elisp")
  (add-to-list 'load-path "~/var/distel/elisp"))
(autoload 'distel-setup "distel" nil t)

(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      dabbrev-expand)
    ("\M-/"      dabbrev-expand)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(eval-after-load "erlang"
  '(progn
     (distel-setup)
     (define-key erlang-extended-mode-map (kbd "RET") 'newline-and-indent)
     (define-key erlang-extended-mode-map (kbd "M-/") 'dabbrev-expand)
     (add-hook 'erlang-shell-mode-hook
               (lambda ()
                 ;; add some Distel bindings to the Erlang shell
                 (dolist (spec distel-shell-keys)
                   (define-key erlang-shell-mode-map (car spec) (cadr spec)))))
     (add-hook 'erlang-mode-hook
               (lambda ()
                 ;; when starting an Erlang shell in Emacs, default in the node name
                 (setq inferior-erlang-machine-options '("-sname" "emacs"))))))

;; Snippets

(require 'yasnippet nil t)
(eval-after-load "yasnippet"
'(progn
   (global-set-key (kbd "C-/") '(lambda () ()))
   (setq
    yas/trigger-key (kbd "C-/")
    yas/next-field-key (kbd "C-/"))
   (yas/initialize)
   (yas/load-directory "~/.emacs.d/snippets/")))


;; stardict
(autoload 'sdcv-search "sdcv-mode" nil t)
(setq sdcv-dictionary-list
      '("Universal (Ru-En)" "LingvoUniversal (En-Ru)" "Computers (En-Ru)"
        "Universal (Ru-En)" "LingvoUniversal (Ru-En)" "Computers (Ru-En)"))
(global-set-key (kbd "C-c d") 'sdcv-search)


;; highlight parentheses
(autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
(dolist (hook '(python-mode-hook
                emacs-lisp-mode-hook))
  (add-hook hook 'highlight-parentheses-mode))
