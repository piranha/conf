;;; modes.el -- various modes configuration

;; usual major modes
(add-to-list 'auto-mode-alist '("\\.egg\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.jsm\\'" . js-mode))

(require 'imenu)
(setq imenu-auto-rescan t)

(el-get-bundle! filladapt)
(el-get-bundle! grep+)

(setq tags-file-name (expand-file-name "~/TAGS"))

;; dired
(setq
 dired-bind-jump nil
 dired-omit-extensions '(".pyc" ".elc"))
(autoload 'dired-jump "dired-x" "Jump to dir of current file" t)
(autoload 'dired-omit-mode "dired-x" "Omit unnecessary files in dired view" t)
(add-hook 'dired-mode-hook 'dired-omit-mode)
(add-hook 'dired-omit-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M-o") nil)
            (define-key dired-mode-map (kbd "M-O") 'dired-omit-mode)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-,")
    (fun-for-bind bs--show-with-configuration "dired")))

(column-number-mode 1)
(show-paren-mode 1)
(global-subword-mode 1)

;; store recent files list
(recentf-mode 1)
(setq recentf-max-menu-items 200
      recentf-max-saved-items 200)
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

(el-get-bundle! flx)
(with-eval-after-load 'flx
  ;; disable ido faces to see flx highlights.
  (setq ido-use-faces nil)
  (flx-ido-mode 1))

(winner-mode 1) ;; window configuration undo/redo

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
(setq save-abbrevs nil)

(which-function-mode)

;; various visuals

(require 'whitespace)
(setq whitespace-style '(face trailing tabs lines-tail))
(setq whitespace-line-column 80)
(set-face-attribute 'whitespace-line nil
                    :foreground 'unspecified
                    :background "yellow")
(add-hook 'prog-mode-hook 'whitespace-mode)

(setq hooks-want-short-lines
      '(markdown-mode-hook
        wikipedia-mode-hook
        rst-mode-hook))
(dolist (hook hooks-want-short-lines)
  (add-hook hook 'auto-fill-mode))

;; highlighting of FIXME/TODO
(el-get-bundle! fic-ext-mode)
(with-eval-after-load 'fic-ext-mode
  (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "NOTE"))
  (dolist (hook '(python-mode-hook
                  emacs-lisp-mode-hook
                  coffee-mode-hook
                  go-mode-hook
                  js-mode-hook
                  clojure-mode-hook
                  web-mode-hook))
    (add-hook hook 'fic-ext-mode)))

;; org mode

(defvar org-hide-leading-stars)
(defvar org-time-clocksum-format)
(defvar org-mode-map)
(add-hook 'org-mode-hook
          (lambda ()
            (setq org-hide-leading-stars t)
            (setq org-time-clocksum-format
                  '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
            (define-key org-mode-map (kbd "C-,") nil)
            (define-key org-mode-map (kbd "C-S-<up>") 'org-timestamp-up)
            (define-key org-mode-map (kbd "C-S-<down>") 'org-timestamp-down)))

;; major modes

(el-get-bundle markdown-mode
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
(defvar markdown-mode-map)
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "M-<right>") nil)
  (define-key markdown-mode-map (kbd "M-<left>") nil))

(el-get-bundle go-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'whitespace-style)
                 '(face trailing lines-tail))))

(el-get-bundle wikipedia-mode)
(el-get-bundle yaml-mode)
(el-get-bundle sass-mode)
(el-get-bundle less-css-mode)
(el-get-bundle coffee-mode)
(el-get-bundle htmlize)
(el-get-bundle po-mode)
(el-get-bundle gist:1654113:go-template-mode)

;;;;;;;;;
;; Python
;;;;;;;;;

;; modified and stored in my repo
(autoload 'django-html-mode "django-html-mode" "Django HTML templates" t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . django-html-mode))

(defvar django-html-mode-map)
(with-eval-after-load 'django-html-mode
  (define-key django-html-mode-map "\C-c]" 'django-html-close-tag)
  (define-key django-html-mode-map (kbd "C-t") "{%  %}\C-b\C-b\C-b")
  (define-key django-html-mode-map (kbd "M-t") "{{  }}\C-b\C-b\C-b")
  (define-key django-html-mode-map (kbd "C-'") "<%  %>\C-b\C-b\C-b")
  (define-key django-html-mode-map (kbd "M-'") "<%=  %>\C-b\C-b\C-b"))

(defvar python-mode-map)
(eval-after-load "python"
  '(define-key python-mode-map (kbd "RET") 'newline-maybe-indent))

(add-hook 'python-mode-hook (lambda () (setq imenu-create-index-function
                                        'python-imenu-create-index)))

(el-get-bundle s)
(el-get-bundle popup)

(el-get-bundle flycheck
  (add-hook 'after-init-hook 'global-flycheck-mode))

(el-get-bundle flycheck-pos-tip)

(el-get-bundle! flycheck-pyflakes in Wilfred/flycheck-pyflakes)

(defvar flycheck-disabled-checkers)
(defvar flycheck-highlighting-mode)
(with-eval-after-load 'flycheck
  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint)
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp)
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))



;; Lua

(el-get-bundle lua-mode)
(defvaralias 'lua-indent-level 'tab-width)

;; Ruby

(defvar ruby-mode-map)
(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent))

;; Javascript

(defvar js-mode-map)
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "RET") 'newline-maybe-indent))

;; Snippets

(el-get-bundle! yasnippet)
(defvar yas-minor-mode-map)
(defvar yas/snippet-dirs)
(with-eval-after-load 'yasnippet
  (add-hook 'snippet-mode-hook
            '(lambda ()
               (set (make-local-variable 'require-final-newline) nil)))
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-/") 'yas-expand)
  (add-to-list 'yas/snippet-dirs "~/.emacs.d/snippets/")
  (yas-global-mode 1))


;; highlight parentheses
(el-get-bundle highlight-parentheses
  (dolist (hook '(python-mode-hook
                  emacs-lisp-mode-hook
                  clojure-mode-hook))
    (add-hook hook 'highlight-parentheses-mode)))

;; version control/projects

(el-get-bundle ag)

(el-get-bundle! projectile)
(defvar projectile-completion-system)
(defvar projectile-enable-caching)
(with-eval-after-load 'projectile
  (projectile-global-mode)
  (setq projectile-completion-system 'ido)
  (setq projectile-enable-caching t))

;; smex
(el-get-bundle! smex
  (setq smex-save-file "~/.emacs.d/smex.save")
  (smex-initialize)
  (smex-auto-update)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; sudo
(el-get-bundle! sudo-save in emacswiki:sudo-save)

;; elisp
(defun lambda-elisp-mode-hook ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(add-hook 'emacs-lisp-mode-hook 'lambda-elisp-mode-hook)

;; breadcrumbs
(el-get-bundle! breadcrumb)
(with-eval-after-load 'breadcrumb
  (global-set-key (kbd "C-'") 'bc-set)
  (global-set-key (kbd "M-j") 'bc-previous)
  (global-set-key (kbd "M-J") 'bc-next)
  (global-set-key (kbd "C-c j") 'bc-goto-current)
  (global-set-key (kbd "C-c C-j") 'bc-list))

;; whole-line-or-region
(el-get-bundle! whole-line-or-region)
(with-eval-after-load 'whole-line-or-region
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

            (whole-line-or-region-mode))

(el-get-bundle browse-kill-ring)

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

;; (el-get-bundle smooth-scroll
;;   :after (smooth-scroll-mode t))

(el-get-bundle! smooth-scrolling)

(el-get-bundle! multi-mode
  :type http
  :url "http://www.gerd-neugebauer.de/software/emacs/multi-mode/multi-mode.el")
(with-eval-after-load 'multi-mode
  (defun better-django-html-mode () (interactive)
         (multi-mode 1
                     'django-html-mode
                     '("<script type=\"text/javascript\">" js-mode)
                     '("</script>" django-html-mode)
                     '("<style type=\"text/css\">" css-mode)
                     '("</style>" django-html-mode)))

  (delete '("\\.html\\'" . django-html-mode) auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . better-django-html-mode)))

(el-get-bundle clojure-mode
  (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs\\.hl\\'" . clojure-mode)))
(defvar clojure-mode-map)
(defvar clojure-defun-style-default-indent)
(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "C-=") 'phoenix-reload)
  (setq clojure-defun-style-default-indent t))

(el-get-bundle peg)
(el-get-bundle edn)

(el-get-bundle clj-refactor)
(with-eval-after-load 'clojure-mode
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c r"))

(el-get-bundle align-cljlet)
(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "C-c r a l") 'align-cljlet))

(el-get-bundle cider
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode))
(defvar cider-mode-map)
(defvar cider-repl-history-file)
(with-eval-after-load 'cider-mode
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (define-key cider-mode-map (kbd "C-=") 'phoenix-reload))

(el-get-bundle rainbow-delimiters
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode))

(defvar filladapt-token-table)
(add-hook 'html-mode-hook
          (lambda ()
            ;; (if (string-prefix-p "/Users/piranha/dev/work"
            ;;                      (buffer-file-name))
            ;;     (set (make-local-variable 'sgml-basic-offset) 4))
            (require 'filladapt)
            (set (make-local-variable 'filladapt-token-table)
                 (append filladapt-token-table
                         '(("<li>[ \t]" bullet))))))

(el-get-bundle paredit
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(defvar paredit-mode-map)
(with-eval-after-load 'paredit
  (dolist (kv '(("C-<right>" nil)
                ("C-<left>" nil)
                ("C-M-0" paredit-forward-slurp-sexp)
                ("C-M-9" paredit-forward-barf-sexp)))
    (define-key paredit-mode-map (kbd (car kv)) (cadr kv))))

(el-get-bundle emacswiki:sql-indent
  (with-eval-after-load 'sql
    (require 'sql-indent)))

;; ;; OCaml


(el-get-bundle tuareg-mode
  (autoload 'ocp-setup-indent "ocp-indent.el" "" t)
  ;;(autoload 'ocp-index-mode "ocp-index.el" "" t)
  (add-hook 'tuareg-mode-hook 'ocp-setup-indent)
  (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu))

(el-get-bundle tuareg-imenu
 :type http
 :url "http://aspellfr.free.fr/tuareg-imenu/tuareg-imenu.el")

(el-get-bundle elpa:merlin
  (add-hook 'tuareg-mode-hook 'merlin-mode))
(defvar merlin-mode-map)
(defvar merlin-use-auto-complete-mode)
(defvar merlin-error-after-save)
(with-eval-after-load 'merlin
  (setq merlin-use-auto-complete-mode nil)
  (setq merlin-error-after-save nil)
  (define-key merlin-mode-map (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
  (define-key merlin-mode-map (kbd "C-c <down>") 'merlin-type-enclosing-go-down))

(el-get-bundle elpa:utop
  ;;(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
  (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer))

(el-get-bundle dockerfile-mode)
(el-get-bundle rust-mode)
(el-get-bundle elpa:toml-mode)

(el-get-bundle web-mode
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode)))

(defvar web-mode-map)
(defvar web-mode-markup-indent-offset)
(defvar web-mode-code-indent-offset)
(defvar web-mode-content-types-alist)
(with-eval-after-load 'web-mode
  (define-key web-mode-map (kbd "C-c /") 'web-mode-element-close)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))
  (setq web-mode-content-types-alist '(;;("jsx" . "\\.js\\'")
                                       ("jsx" . "\\.react\\.js\\'"))))

(el-get-bundle magit
  :branch "2.1.0"
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(with-eval-after-load 'magit
  (add-to-list 'magit-diff-arguments "--no-ext-diff")
  (add-to-list 'magit-diff-section-arguments "--no-ext-diff"))

(el-get-bundle ace-jump-mode
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

(el-get-bundle! string-inflection in akicho8/string-inflection)

(el-get-bundle piu
  :url "http://paste.in.ua/piu.el"
  (global-set-key (kbd "C-x p") 'piu))
