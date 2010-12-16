;; various small modes configuration

;(autoload 'gnus "gnus" "Best email client ever" t)

(require 'imenu)

(el-get-add
 (:name filladapt
  :after (lambda () (autoload 'filladapt-mode "filladapt" nil t))))

(el-get-add
 (:name grep+
  :type http
  :url "http://www.emacswiki.org/emacs/download/grep%2b.el"
  :after (lambda () (autoload 'grep "grep+" nil t))))

(setq tags-file-name (expand-file-name "~/TAGS"))

(el-get-add
 (:name session
  :after (lambda () (autoload 'session-initialize "session" nil t)
           (add-hook 'after-init-hook 'session-initialize))))

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

;; hello IDO, you are very nice
(ido-mode 1)
(setq
 ido-enable-flex-matching t
 ido-show-dot-for-dired t
 ido-auto-merge-work-directories-length -1 ; disable auto-merging
 ido-confirm-unique-completion t)

(winner-mode 1) ;; window configuration undo/redo

(el-get-add
 (:name pointback
  :type http
  :url "http://stud1.tuwien.ac.at/~e0225855/pointback/pointback.el"
  :after (lambda () (require 'pointback)
           (global-pointback-mode))))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; saving place in file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/places")

(which-function-mode)

;; various visuals

(require 'whitespace)
(setq whitespace-style '(face trailing tabs lines-tail))
(setq whitespace-line-column 120)
(set-face-attribute 'whitespace-line nil
                    :foreground 'unspecified
                    :background "yellow")

(setq hooks-with-whitespaces
      '(wikipedia-mode-hook
        markdown-mode-hook
        erlang-mode-hook
        haskell-mode-hook
        python-mode-hook
        lua-mode-hook
        coffee-mode-hook
        js-mode-hook))
(dolist (hook hooks-with-whitespaces) (add-hook hook 'whitespace-mode))

(setq hooks-want-short-lines
      '(markdown-mode-hook
        wikipedia-mode-hook
        rst-mode-hook))
(dolist (hook hooks-want-short-lines)
  (add-hook hook 'auto-fill-mode))

;; FIXME/TODO highlighting
(el-get-add
 (:name fic-ext-mode
  :type http
  :url "http://www.emacswiki.org/emacs/download/fic-ext-mode.el"
  :features turn-on-fic-ext-mode
  :after (lambda ()
           (dolist (hook '(python-mode-hook
                           emacs-lisp-mode-hook))
             (add-hook hook 'turn-on-fic-ext-mode)))))

;; major modes

(el-get-add
 (:name markdown-mode
  :type http
  :url "http://jblevins.org/projects/markdown-mode/markdown-mode.el"
  :features markdown-mode
  :after (lambda ()
           (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))))

(el-get-add
 (:name wikipedia-mode
  :type http
  :url "http://www.emacswiki.org/cgi-bin/wiki/download/wikipedia-mode.el"
  :features wikipedia-mode
  :after (lambda ()
           (add-to-list 'auto-mode-alist
                        '("\\.wiki\\.txt\\'" . wikipedia-mode)))))

(el-get-add
 (:name yaml-mode
  :type git
  :url "git://github.com/yoshiki/yaml-mode.git"
  :features yaml-mode
  :after (lambda ()
           (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
           (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))))

(el-get-add
 (:name lua-mode
  :type http
  :url "http://luaforge.net/plugins/scmcvs/cvsweb.php/lua-mode/lua-mode.el?rev=HEAD;content-type=text%2Fplain;cvsroot=lua-mode"
  :after (lambda () (autoload 'lua-mode "lua-mode" nil t)
           (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))))

(el-get-add
 (:name clevercss
  :type git
  :url "git://github.com/jschaf/CleverCSS-Mode.git"
  :after (lambda () (autoload 'clevercss-mode "clevercss" nil t)
           (add-to-list 'auto-mode-alist
                        '("\\.ccss\\'" . clevercss-mode)))))

;; modified and stored in my repo
(autoload 'django-html-mode "django-html-mode" "Django HTML templates" t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . django-html-mode))

(add-to-list 'auto-mode-alist '("\\.egg\\'" . archive-mode))


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

(el-get-add
 (:name flymake-point
  :type http
  :url "http://bitbucket.org/brodie/dotfiles/raw/tip/.emacs.d/plugins/flymake-point.el"
  :after (lambda () (require 'flymake-point))))

;; Lua

(defvaralias 'lua-indent-level 'tab-width)

;; Ruby

(eval-after-load "ruby-mode"
  '(progn
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))

;; Javascript

(eval-after-load "js"
  '(progn
     (define-key js-mode-map (kbd "RET") 'newline-maybe-indent)))

;; Coffee

(el-get-add
 (:name coffee-mode
  :type git
  :url "git://github.com/defunkt/coffee-mode.git"
  :features coffee-mode
  :after (lambda ()
           (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
           (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
           (setq coffee-js-mode 'javascript-mode)
           (eval-after-load "coffee-mode"
             '(define-key coffee-mode-map (kbd "RET") 'newline-maybe-indent)))))

;; Snippets

(el-get-add
 (:name yasnippet
  :after (lambda () (require 'yasnippet nil t)
           (eval-after-load "yasnippet"
             '(progn
                (global-unset-key (kbd "C-/"))
                (setq
                 yas/trigger-key (kbd "C-/")
                 yas/next-field-key (kbd "C-/"))
                (yas/initialize)
                (yas/load-directory "~/.emacs.d/snippets/"))))))


;; highlight parentheses
(el-get-add
 (:name highlight-parentheses
  :after (lambda () (autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
           (dolist (hook '(python-mode-hook
                           emacs-lisp-mode-hook))
             (add-hook hook 'highlight-parentheses-mode)))))

;; version control/projects

(el-get-add
 (:name ack
  :type http
  :url "http://repo.or.cz/w/ShellArchive.git?a=blob_plain;hb=HEAD;f=ack.el"
  :features ack))

(el-get-add
 (:name project-root
  :type hg
  :url "http://hg.piranha.org.ua/project-root/"
  :features project-root
  :after (lambda ()
           (global-set-key (kbd "C-c p f") 'project-root-find-file)
           (global-set-key (kbd "C-c p g") 'project-root-grep)
           (global-set-key (kbd "C-c p a") 'project-root-ack)
           (global-set-key (kbd "C-c p d") 'project-root-goto-root)
           (global-set-key (kbd "C-c p l") 'project-root-browse-seen-projects)
           (global-set-key (kbd "C-c p b") 'project-root-switch-buffer)

           (setq project-roots
                 `(("Paylogic project"
                    :root-contains-files ("../paylogic/" "../fabfile.py")
                    :filename-regex ,(regexify-ext-list '(py html css js sh)))
                   ("Svarga project"
                    :root-contains-files ("manage.py" "venv")
                    :filename-regex ,(regexify-ext-list '(py html css js sh ccss))
                    :exclude-paths '("venv" "docs/_build"))
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
                    :root-contains-files (".git")))))))


;; smex
(el-get-add
 (:name smex
  :type git
  :url "git://github.com/nonsequitur/smex.git"
  :features smex
  :after (lambda ()
           (setq smex-save-file "~/.emacs.d/smex.save")
           (smex-initialize)
           (smex-auto-update)
           (global-set-key (kbd "M-x") 'smex)
           (global-set-key (kbd "M-X") 'smex-major-mode-commands)
           (global-set-key (kbd "C-c M-x") 'smex-update-and-run)
           (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))))

;; sudo
(el-get-add
 (:name sudo-save
  :type http
  :url "http://www.emacswiki.org/emacs/download/sudo-save.el"
  :features sudo-save))

;; elisp
(defun lambda-elisp-mode-hook ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(add-hook 'emacs-lisp-mode-hook 'lambda-elisp-mode-hook)

;; highlight
(el-get-add
 (:name rainbow-mode
  :after (lambda () (rainbow-mode))))

;; breadcrumbs
(el-get-add
 (:name breadcrumb
  :type http
  :url "http://downloads.sourceforge.net/project/breadcrumbemacs/Breadcrumb%20for%20Emacs/1.1.3/breadcrumb-1.1.3.zip"
  :build ("unzip breadcrumb-1.1.3.zip")
  :features breadcrumb
  :after (lambda ()
           (global-set-key [?\S- ] 'bc-set) ;; Shift-SPACE
           (global-set-key (kbd "M-j") 'bc-previous)
           (global-set-key (kbd "M-J") 'bc-next)
           (global-set-key (kbd "C-c j") 'bc-goto-current)
           (global-set-key (kbd "C-c M-j") 'bc-list))))

;; whole-line-or-region
(el-get-add
 (:name whole-line-or-region
  :type http
  :url "http://www.northbound-train.com/emacs/whole-line-or-region.el"
  :features whole-line-or-region
  :after (lambda ()
           (defun whole-line-or-region-comment-dwim (prefix)
             "Call `comment-dwim' on current region or current line."
             (interactive "*p")
             (whole-line-or-region-call-with-prefix 'comment-dwim prefix nil t))

           (add-to-list 'whole-line-or-region-extensions-alist
                        '(comment-dwim whole-line-or-region-comment-dwim))
           (whole-line-or-region-mode))))
