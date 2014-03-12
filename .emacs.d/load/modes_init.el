;; various modes configuration

;; usual major modes
(add-to-list 'auto-mode-alist '("\\.egg\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.jsm\\'" . js-mode))

(require 'imenu)
(setq imenu-auto-rescan t)

(el-get-add
 (:name filladapt
  :after (progn (autoload 'filladapt-mode "filladapt" nil t))))

(el-get-add
 (:name grep+
  :features grep+))

(setq tags-file-name (expand-file-name "~/TAGS"))

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
(setq recentf-max-menu-items 2000
      recentf-max-saved-items 2000)
(global-set-key (kbd "C-c C-f")
                '(lambda ()
                   (interactive)
                   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
                       (message "Opening file...")
                     (message "Aborting"))))

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
 ido-confirm-unique-completion t
 ido-ignore-files '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./"
                    "\\.pyc$" "\\.6$" "\\.o$" "__pycache__/$"))
(defalias 'list-buffers 'ido-switch-buffer)

(el-get-add
 (:name flx
  :type git
  :url "https://github.com/lewang/flx"
  :features flx-ido
  :after (progn
           (flx-ido-mode 1)
           ;; disable ido faces to see flx highlights.
           (setq ido-use-faces nil))))

(winner-mode 1) ;; window configuration undo/redo

(el-get-add
 (:name pointback
  :features pointback
  :after (progn (global-pointback-mode))))

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; saving place in file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/places")

;; saving history
(savehist-mode 1)
(setq savehist-additional-variables
      '(search-ring
        regexp-search-ring
        kill-ring
        file-name-history
        command-history
        shell-command-history))

(which-function-mode)

;; various visuals

(require 'whitespace)
(setq whitespace-style '(face trailing tabs lines-tail))
(setq whitespace-line-column 80)
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
        js-mode-hook
        go-mode-hook))
(dolist (hook hooks-with-whitespaces) (add-hook hook 'whitespace-mode))

(setq hooks-want-short-lines
      '(markdown-mode-hook
        wikipedia-mode-hook
        rst-mode-hook))
(dolist (hook hooks-want-short-lines)
  (add-hook hook 'auto-fill-mode))

;; highlighting of FIXME/TODO
(el-get-add
 (:name fic-ext-mode
  :features fic-ext-mode
  :after (progn
           (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "NOTE"))
           (dolist (hook '(python-mode-hook
                           emacs-lisp-mode-hook
                           coffee-mode-hook
                           go-mode-hook
                           js-mode-hook
                           clojure-mode-hook))
             (add-hook hook 'fic-ext-mode)))))

;; org mode

(add-hook 'org-mode-hook
          (lambda ()
            (setq org-hide-leading-stars t)))

;; major modes

(el-get-add
 (:name markdown-mode
  :after (progn
           (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
           (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))))

(el-get-add
 (:name wikipedia-mode))

(el-get-add
 (:name yaml-mode))

(setq save-abbrevs nil)
(el-get-add
 (:name lua-mode))

(el-get-add
 (:name clevercss))

;; modified and stored in my repo
(autoload 'django-html-mode "django-html-mode" "Django HTML templates" t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . django-html-mode))

(el-get-add
 (:name go-mode))

(el-get-add
 (:name haml-mode))

(el-get-add
 (:name sass-mode
  :after (progn
           (add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode)))))

;;;;;;;;;
;; Python
;;;;;;;;;

(eval-after-load "django-html-mode"
  '(progn
     (define-key django-html-mode-map "\C-c]" 'django-html-close-tag)
     (define-key django-html-mode-map (kbd "C-t") "{%  %}\C-b\C-b\C-b")
     (define-key django-html-mode-map (kbd "M-t") "{{  }}\C-b\C-b\C-b")
     (define-key django-html-mode-map (kbd "C-'") "<%  %>\C-b\C-b\C-b")
     (define-key django-html-mode-map (kbd "M-'") "<%=  %>\C-b\C-b\C-b")
     ))

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

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'python-mode-hook '(lambda () (flymake-mode 1)))

(push '("\\([^:]+\\):\\([0-9]+\\)\\(([0-9]+)\\)?: \\[.\\] \\(.*\\)"
        1 2 3 4)
      flymake-err-line-patterns)

(set-face-attribute 'flymake-errline nil
                    :background 'unspecified
                    :underline "orange")

(el-get-add
 (:name flymake-lua))

(el-get-add
 (:name flymake-point
  :features flymake-point))

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

;; argh, this one wants 'npm install formidable'
(el-get-add
 (:name jshint-mode
  :type git
  :url "https://github.com/daleharvey/jshint-mode.git"
  :features flymake-jshint
  :after (progn (add-hook 'js-mode-hook '(lambda () (flymake-mode 1))))))

;; Coffee

(el-get-add
 (:name coffee-mode
  :after (progn
           (add-to-list 'auto-mode-alist '("\\.coffeex\\'" . coffee-mode))
           (add-hook 'coffee-mode-hook
                     (lambda ()
                       (define-key coffee-mode-map (kbd "C-]") "@$."))))))

;; Snippets

(el-get-add
 (:name yasnippet
  :after (progn (require 'yasnippet nil t)
           (eval-after-load "yasnippet"
             '(progn
                (add-hook 'snippet-mode-hook
                          '(lambda ()
                             (set (make-local-variable 'require-final-newline) nil)))
                (define-key yas-minor-mode-map [(tab)] nil)
                (define-key yas-minor-mode-map (kbd "TAB") nil)
                (add-to-list 'yas/snippet-dirs
                             "~/.emacs.d/snippets/")
                (yas-global-mode 1))))))


;; highlight parentheses
(el-get-add
 (:name highlight-parentheses
  :after (progn (autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
           (dolist (hook '(python-mode-hook
                           emacs-lisp-mode-hook))
             (add-hook hook 'highlight-parentheses-mode)))))

;; version control/projects

(el-get-add
 (:name ack))

(el-get-add
 (:name project-root
  :type hg
  :url "http://hg.piranha.org.ua/project-root/"
  :features project-root
  :after (progn
           (defun goreplace ()
             (interactive)
             (let ((grep-command "gr"))
               (call-interactively 'grep)))

           (defun project-root-goreplace ()
             (interactive)
               (with-project-root (call-interactively 'goreplace)))

           (global-set-key (kbd "C-c p f") 'project-root-find-file)
           (global-set-key (kbd "C-c p g") 'project-root-grep)
           (global-set-key (kbd "C-c p a") 'project-root-goreplace)
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
                    :exclude-paths '("_build"))
                   ("webcs"
                    :root-contains-files ("Makefile" "config.js")
                    :filename-regex ,(regexify-ext-list
                                      '(coffee eco less json html js))
                    :exclude-paths ("_build" "_www" "node_modules"))
                   ("Generic Mercurial project"
                    :root-contains-files (".hg"))
                   ("Generic git project"
                    :root-contains-files (".git"))
                   ("Generic Clojure project"
                    :root-contains-files ("project.clj")
                    :exclude-paths ("out" ".repl" "target")))))))

;; smex
(el-get-add
 (:name smex
  :features smex
  :after (progn
           (setq smex-save-file "~/.emacs.d/smex.save")
           (smex-initialize)
           (smex-auto-update)
           (global-set-key (kbd "M-x") 'smex)
           (global-set-key (kbd "M-X") 'smex-major-mode-commands)
           (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))))

;; sudo
(el-get-add
 (:name sudo-save
  :type emacswiki
  :features sudo-save))

;; elisp
(defun lambda-elisp-mode-hook ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(add-hook 'emacs-lisp-mode-hook 'lambda-elisp-mode-hook)

;; breadcrumbs
(el-get-add
 (:name breadcrumb
  :features breadcrumb
  :after (progn
           (global-set-key (kbd "C-'") 'bc-set)
           (global-set-key (kbd "M-j") 'bc-previous)
           (global-set-key (kbd "M-J") 'bc-next)
           (global-set-key (kbd "C-c j") 'bc-goto-current)
           (global-set-key (kbd "C-c C-j") 'bc-list))))

;; whole-line-or-region
(el-get-add
 (:name whole-line-or-region
  :features whole-line-or-region
  :after (progn
           (defun whole-line-kill-region-or-word-backward (prefix)
             "Kill (cut) region or just a single word backward"
             (interactive "*p")
             (if (not (and mark-active (/= (point) (mark))))
                 (subword-backward-kill prefix)
               (whole-line-or-region-call-with-region 'kill-region prefix t)))

           (setq whole-line-or-region-extensions-alist
                 '((comment-dwim whole-line-or-region-comment-dwim)
                   (copy-region-as-kill whole-line-or-region-copy-region-as-kill nil)
                   (kill-region whole-line-kill-region-or-word-backward nil)
                   (kill-ring-save whole-line-or-region-kill-ring-save nil)
                   (yank whole-line-or-region-yank nil)))

           (whole-line-or-region-mode))))

(el-get-add
 (:name browse-kill-ring))

;; VC mode
;; do not ever annoy me with slow file loading time
(remove-hook 'find-file-hook 'vc-find-file-hook)

;; smerge
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)
(setq smerge-command-prefix (kbd "C-c ]"))

(el-get-add
 (:name puppet-mode))

(el-get-add
 (:name smooth-scroll
  :after (progn
           (smooth-scroll-mode t))))

(el-get-add
 (:name multi-mode
  :type http
  :url "http://www.tex.ac.uk/ctan/support/iso-tex/multi-mode.el"
  :features multi-mode
  :after (progn
           (defun enyo-mode () (interactive)
             (multi-mode 1
                         'coffee-mode
                         '("YAML.parse('''" yaml-mode)
                         '("''')" coffee-mode)))

           (defun better-django-html-mode () (interactive)
             (multi-mode 1
                         'django-html-mode
                         '("<script type=\"text/javascript\">" js-mode)
                         '("</script>" django-html-mode)
                         '("<style type=\"text/css\">" css-mode)
                         '("</style>" django-html-mode)))

           (delete '("\\.html\\'" . django-html-mode) auto-mode-alist)
           (add-to-list 'auto-mode-alist '("\\.html\\'" . better-django-html-mode))

           (delete '("\\.html?\\'" flymake-xml-init)
                   flymake-allowed-file-name-masks))))

;; go
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'whitespace-style)
                 '(face trailing lines-tail))))

(el-get-add
 (:name highlight-symbol
  :after (progn
           (global-set-key (kbd "C-=") 'highlight-symbol-at-point)
           (global-set-key (kbd "C-c ]") 'highlight-symbol-next)
           (global-set-key (kbd "C-c [") 'highlight-symbol-prev))))

(el-get-add
 (:name less-css-mode
  :type git
  :url "https://github.com/purcell/less-css-mode/"
  :load "less-css-mode.el"))

(el-get-add
 (:name deft
  :type http
  :url "http://jblevins.org/projects/deft/deft.el"
  :features deft
  :after (progn
           (setq deft-directory "~/Dropbox/PlainText/")
           (setq deft-text-mode 'markdown-mode))))


(el-get-add
 (:name htmlize))

(add-to-list 'auto-mode-alist '("\\.eco\\'" . django-html-mode))

(el-get-add
 (:name clojure-mode
  :after (progn
           (add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))
           (add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))
           (add-hook 'clojure-mode-hook
                     (lambda () (define-clojure-indent
                             (expect 'defun)
                             (expect-let 'defun)
                             (given 'defun)
                             (context 1)
                             (freeze-time 1)
                             (redef-state 1)
                             (from-each 1)))))))

(el-get-add
 (:name clj-refactor
  :after (add-hook 'clojure-mode-hook
                   (lambda ()
                     (clj-refactor-mode 1)
                     (cljr-add-keybindings-with-prefix "C-c r")))))

(el-get-add
 (:name align-cljlet
  :after (add-hook 'clojure-mode-hook
                   (lambda ()
                     (define-key clojure-mode-map (kbd "C-c r a l")
                       'align-cljlet)))))

(el-get-add
 (:name cider
  :after (progn
           (setq cider-repl-history-file "~/.emacs.d/cider-history")
           (add-hook 'cider-repl-mode-hook 'paredit-mode)
           (add-hook 'clojure-mode-hook 'cider-mode))))

(el-get-add
 (:name rainbow-delimiters
  :after (progn
           (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
           (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode))))

(el-get-add
 (:name zencoding-mode
  :after (progn
           (add-hook 'sgml-mode-hook 'zencoding-mode))))

(add-hook 'html-mode-hook
          (lambda ()
            ;; (if (string-prefix-p "/Users/piranha/dev/work"
            ;;                      (buffer-file-name))
            ;;     (set (make-local-variable 'sgml-basic-offset) 4))
            (require 'filladapt)
            (set (make-local-variable 'filladapt-token-table)
                 (append filladapt-token-table
                         '(("<li>[ \t]" bullet))))))

(el-get-add
 (:name paredit
  :after (progn (dolist (hook '(clojure-mode-hook
                                emacs-lisp-mode-hook))
                  (add-hook hook 'paredit-mode))
                (add-hook 'paredit-mode-hook
                          (lambda ()
                            (dolist (kv '(("C-<right>" nil)
                                          ("C-<left>" nil)
                                          ("C-M-0" paredit-forward-slurp-sexp)
                                          ("C-M-9" paredit-forward-barf-sexp)))
                              (define-key paredit-mode-map (kbd (car kv)) (cadr kv))))))))

(el-get-add
 (:name go-template-mode
  :type git
  :url "git://gist.github.com/1654113.git"
  :features go-template-mode))

(el-get-add
 (:name web-mode
  :type git
  :url "https://github.com/fxbois/web-mode"
  :features web-mode))

(el-get-add
 (:name po-mode))

(el-get-add
 (:name sql-indent
  :type emacswiki
  :after (eval-after-load "sql"
           (load-library "sql-indent"))))

(el-get-add
 (:name smartscan
  :type git
  :url "https://github.com/mickeynp/smart-scan"
  :features smartscan
  :after (global-smartscan-mode 1)))

(el-get-add
 (:name wakatime-mode
        :type git
        :features wakatime-mode
        :url "https://github.com/wakatime/wakatime-mode"))
