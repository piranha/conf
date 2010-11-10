;; various small modes configuration

;(autoload 'gnus "gnus" "Best email client ever" t)

(require 'imenu)
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
(show-paren-mode 1)
(global-subword-mode 1)

;; store recent files list
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-c C-r") 'recentf-open-files)

;; I hate blinking
(if (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; It's annoying
(if (fboundp 'global-semantic-stickyfunc-mode)
    (global-semantic-stickyfunc-mode -1))

;; highlight marked text
(transient-mark-mode 1)
;; but work even without it
(setq mark-even-if-inactive t)

;; IDO, you are very nice
(ido-mode 1)
(setq
 ido-enable-flex-matching t
 ido-show-dot-for-dired t
 ido-auto-merge-work-directories-length -1 ; disable auto-merging
 ido-confirm-unique-completion t)

(winner-mode 1) ;; window configuration undo/redo
(require 'pointback)
(global-pointback-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; saving place in file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/places/")

;; major modes

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(autoload 'wikipedia-mode "wikipedia-mode" "Major mode for editing MediaWiki files" t)
(autoload 'django-html-mode "django-html-mode" "Django HTML templates" t)
(autoload 'haskell-mode "haskell-mode" "Major mode for editing Haskell sources" t)
(autoload 'io-mode "io-mode" "Major mode for editing Io sources" t)
(autoload 'yaml-mode "yaml-mode" nil t)
(autoload 'lua-mode "lua-mode" nil t)
(autoload 'clevercss-mode "CleverCSS-Mode/clevercss" nil t)

(require 'whitespace)
(setq whitespace-style '(face trailing tabs lines-tail))
(setq whitespace-line-column 120)
(set-face-attribute 'whitespace-line nil
                    :foreground 'unspecified
                    :background "yellow")

(setq hooks-with-whitespaces
      '(factor-mode-hook
        wikipedia-mode-hook
        markdown-mode-hook
        erlang-mode-hook
        haskell-mode-hook
        python-mode-hook
        lua-mode-hook
        coffee-mode-hook))
(dolist (hook hooks-with-whitespaces) (add-hook hook 'whitespace-mode))

(setq hooks-want-short-lines
      '(markdown-mode-hook
        wikipedia-mode-hook
        rst-mode-hook))
(dolist (hook hooks-want-short-lines)
  (add-hook hook 'auto-fill-mode))

;; FIXME/TODO highlighting
(autoload 'turn-on-fic-mode "fic-mode" "" t)
(dolist (hook '(python-mode-hook
                emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-fic-mode))

(setq auto-mode-alist
      (append
       (list
        '("\\.md\\'" . markdown-mode)
        '("\\.erl\\'" . erlang-mode)
        '("\\.hs\\'" . haskell-mode)
        '("\\.wiki\\.txt\\'" . wikipedia-mode)
        '("\\.html\\'" . django-html-mode)
        '("\\.egg\\'" . archive-mode)
        '("\\.io\\'" . io-mode)
        '("\\.yml\\'" . yaml-mode)
        '("\\.yaml\\'" . yaml-mode)
        '("\\.lua\\'" . lua-mode)
        '("\\.ccss\\'" . clevercss-mode))
        auto-mode-alist))

(ignore-errors (load-file "~/var/factor/misc/fuel/fu1.el"))
(setq fuel-listener-factor-binary "~/var/factor/factor")
(setq fuel-listener-factor-image "~/var/factor/factor.image")
(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map (kbd "C-M-l")
       (fun-for-bind switch-to-buffer (other-buffer)))))

(setq w3m-use-cookies t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;
;; Python
;;;;;;;;;

(eval-after-load "django-html-mode"
  '(progn
     (define-key django-html-mode-map "\C-c]" 'django-html-close-tag)))

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "RET") 'newline-maybe-indent)))

(add-hook 'python-mode-hook (lambda () (setq imenu-create-index-function
                                        'python-imenu-create-index)))

;;;;;;;;;;
;; Flymake
;;;;;;;;;;

(defun buffer-is-local ()
  (if (fboundp 'tramp-list-remote-buffers)
      (not (member (current-buffer) (tramp-list-remote-buffers)))
    t))

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    ;; Make sure it's not a remote buffer or flymake would not work
    (when (buffer-is-local)
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "pyflakes" (list local-file)))))

  (defun flymake-lua-init ()
    "Invoke luac with '-p' to get syntax checking"
    (when (buffer-is-local)
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "luac" (list "-p" local-file)))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.lua\\'" flymake-lua-init))
  (push '("^.*luac[0-9.]*\\(.exe\\)?: *\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 2 3 nil 4)
        flymake-err-line-patterns))

(add-hook 'python-mode-hook '(lambda () (flymake-mode 1)))
(add-hook 'lua-mode-hook '(lambda () (flymake-mode 1)))

(set-face-attribute 'flymake-errline nil
                    :background 'unspecified
                    :underline "orange")
(setq flymake-gui-warnings-enabled nil)

(require 'flymake-point nil t)

;; Lua

(defvaralias 'lua-indent-level 'tab-width)
(add-hook 'lua-mode-hook (lambda () (setq indent-tabs-mode t)))

;; Ruby

(eval-after-load "ruby-mode"
  '(progn
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))

;; Javascript

(eval-after-load "js-mode"
  '(progn
     (define-key js-mode-map (kbd "RET") 'newline-maybe-indent)))
(autoload 'js2-mode "js2-mode" nil t)

;; Coffee

(autoload 'coffee-mode "coffee-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(setq coffee-js-mode 'javascript-mode)
(eval-after-load "coffee-mode"
  '(define-key coffee-mode-map (kbd "RET") 'newline-maybe-indent))

;; Snippets

(require 'yasnippet nil t)
(eval-after-load "yasnippet"
'(progn
   (global-unset-key (kbd "C-/"))
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

;; version control/projects

(require 'ahg nil t)
(setq ahg-global-key-prefix (kbd "C-c h"))

(require 'egg "egg/egg.el" t)

(require 'project-root)
(autoload 'ack "ack" "" t)

(setq project-roots
      `(("Paylogic project"
         :root-contains-files ("../paylogic/" "../fabfile.py")
         :filename-regex ,(regexify-ext-list '(py html css js sh)))
        ("Svarga project"
         :root-contains-files ("manage.py" "venv")
         :filename-regex ,(regexify-ext-list '(py html css js sh))
         :exclude-paths '("venv"))
        ("Django project"
         :root-contains-files ("manage.py")
         :filename-regex ,(regexify-ext-list '(py html css js sh))
         :exclude-paths '("contrib"))
        ("Mercurial"
         :root-contains-files ("hg" "hgeditor")
         :filename-regex ,(regexify-ext-list '(py tmpl))
         :exclude-paths '("tests" "build"))
        ("Sphinx documentation"
         :root-contains-files ("Makefile" "conf.py")
         :filename-regex ,(regexify-ext-list '(py rst))
         :exclude-paths '("_build"))
        ("Python project with buildout"
         :root-contains-files ("../../buildout.cfg")
         :filename-regex ,(regexify-ext-list '(py)))
        ("Generic Python project"
         :root-contains-files ("setup.py")
         :filename-regex ,(regexify-ext-list '(py)))
        ("Ruby web project"
         :root-contains-files ("config.ru")
         :filename-regex ,(regexify-ext-list '(rb ru haml erb css js)))
        ("Cyrax site"
         :root-contains-files ("index.html" "settings.cfg")
         :filename-regex ,(regexify-ext-list '(html js css cfg))
         :exlude-paths '("_build"))
        ("Generic Mercurial project"
         :root-contains-files (".hg"))
        ("Generic git project"
         :root-contains-files (".git"))))

(global-set-key (kbd "C-c p f") 'project-root-find-file)
(global-set-key (kbd "C-c p g") 'project-root-grep)
(global-set-key (kbd "C-c p a") 'project-root-ack)
(global-set-key (kbd "C-c p d") 'project-root-goto-root)
(global-set-key (kbd "C-c p l") 'project-root-browse-seen-projects)
(global-set-key (kbd "C-c p b") 'project-root-switch-buffer)

;; mail
(autoload 'post-mode "post" nil t)
(add-to-list 'auto-mode-alist '("sup\\.\\(compose\\|forward\\|reply\\|resume\\)-mode$" . post-mode))
(add-to-list 'auto-mode-alist '("\\.*mutt-\\.*" . post-mode))
(setq post-emoticon-pattern nil
      post-news-poster-regexp "^Exceprts from .* of .*:$")

;; smex
(if (require 'smex nil t)
    (progn
      (setq smex-save-file "~/.emacs.d/smex.save")
      (smex-initialize)
      (smex-auto-update)
      (global-set-key (kbd "M-x") 'smex)
      (global-set-key (kbd "M-X") 'smex-major-mode-commands)
      (global-set-key (kbd "C-c M-x") 'smex-update-and-run)
      (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))


;; sudo
(autoload 'sudo-unset-ro-or-save "sudo" "" t)
(autoload 'sudo-find-file "sudo" "" t)
(global-set-key (kbd "C-x M-s") 'sudo-unset-ro-or-save)
(global-set-key (kbd "C-x M-f") 'sudo-find-file)

;; elisp
(defun lambda-elisp-mode-hook ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(add-hook 'emacs-lisp-mode-hook 'lambda-elisp-mode-hook)

;; highlight
(autoload 'rainbow-mode "rainbow/rainbow-mode" "" t)
(if (fboundp 'rainbow-mode)
    (rainbow-mode))

;; breadcrumbs
(require 'breadcrumb)
(global-set-key [?\S- ] 'bc-set) ;; Shift-SPACE
(global-set-key (kbd "M-j") 'bc-previous)
(global-set-key (kbd "M-J") 'bc-next)
(global-set-key (kbd "C-c j") 'bc-goto-current)
(global-set-key (kbd "C-c M-j") 'bc-list)

;; whole-line-or-region
(require 'whole-line-or-region nil t)

(defun whole-line-or-region-comment-dwim (prefix)
  "Call `comment-dwim' on current region or current line."
  (interactive "*p")
  (whole-line-or-region-call-with-prefix 'comment-dwim prefix nil t))

(add-to-list 'whole-line-or-region-extensions-alist
             '(comment-dwim whole-line-or-region-comment-dwim))
(whole-line-or-region-mode)
