;;; modes.el -- various modes configuration

;; usual major modes
(add-to-list 'auto-mode-alist '("\\.egg\\'" . archive-mode))

(require 'imenu) ;; NOTE: check out flimenu
(setq imenu-auto-rescan t)

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
            (define-key dired-mode-map (kbd "M-O") 'dired-omit-mode)
            (define-key dired-mode-map (kbd "C-,")
              (lambda () (bs--show-with-configuration "dired")))))

(setq eshell-scroll-to-bottom-on-input 'all
      eshell-error-if-no-glob t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions nil)

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
(ido-mode nil)
(setq
 ido-enable-flex-matching t
 ido-show-dot-for-dired t
 ido-auto-merge-work-directories-length -1 ; disable auto-merging
 ido-confirm-unique-completion t
 ido-ignore-files '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./"
                    "\\.pyc$" "\\.6$" "\\.o$" "__pycache__/$"))
(defalias 'list-buffers 'ido-switch-buffer)

(winner-mode 1) ;; window configuration undo/redo
(windmove-default-keybindings)

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

;; org mode

(add-hook 'org-mode-hook
          (lambda ()
            (setq org-hide-leading-stars t)
            (define-key org-mode-map (kbd "C-,") nil)
            (define-key org-mode-map (kbd "C-S-<up>") 'org-timestamp-up)
            (define-key org-mode-map (kbd "C-S-<down>") 'org-timestamp-down)))

;; major modes

(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" . markdown-mode)
        ("\\.md\\'" . markdown-mode)
  :bind (:map markdown-mode-map
         ("M-<right>" . nil)
         ("M-<left>" . nil)))


(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'whitespace-style)
                   '(face trailing lines-tail)))))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" . yaml-mode))

(use-package sass-mode
  :ensure t
  :mode ("\\.scss" . scss-mode))

(use-package less-css-mode
  :ensure t
  :mode ("\\.less" . less-css-mode))

(use-package po-mode
  :ensure t
  :mode ("\\.po\\'" . po-mode))

;;;;;;;;;
;; Python
;;;;;;;;;

(defvar python-mode-map)
(add-hook 'python-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'python-imenu-create-index)
            (define-key python-mode-map (kbd "RET") 'newline-maybe-indent)))


(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (delete 'python-flake8 flycheck-checkers)
  (delete 'python-pylint flycheck-checkers)
  (delete 'emacs-lisp flycheck-checkers)
  (delete 'emacs-lisp-checkdoc flycheck-checkers))


(use-package flycheck-pos-tip
  :ensure t
  :init
  (setq-default flycheck-display-errors-function 'flycheck-pos-tip-error-messages))


(use-package flycheck-pyflakes
  :ensure t
  :init
  (add-hook 'python-mode-hook (lambda () (require 'flycheck-pyflakes))))


(use-package flycheck-joker
  :ensure t)


;; Ruby

(defvar ruby-mode-map)
(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent))

;; Javascript

(defvar js-mode-map)
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "RET") 'newline-maybe-indent))


;; Snippets

(use-package yasnippet
  :ensure t
  :bind (:map yas-minor-mode-map
         ("C-/" . yas-expand)
         ("TAB" . nil))
  :init
  (add-hook 'snippet-mode-hook
            '(lambda ()
               (set (make-local-variable 'require-final-newline) nil)))
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/"))


;; highlight parentheses
(use-package highlight-parentheses
  :ensure t
  :init
  (dolist (hook '(python-mode-hook
                  emacs-lisp-mode-hook
                  clojure-mode-hook))
    (add-hook hook 'highlight-parentheses-mode)))

;; version control/projects

(use-package ag
  :ensure t)

(defun projectile-selection-at-point ()
  (when (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun projectile-counsel-rg ()
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (counsel-rg (projectile-selection-at-point) dir)
      (message "error: Not in a project"))))

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map)
         :map projectile-command-map
         ("s s" . projectile-counsel-rg))
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (projectile-mode 1))


;; elisp
(defun lambda-elisp-mode-hook ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(add-hook 'emacs-lisp-mode-hook 'lambda-elisp-mode-hook)


;; ;; breadcrumbs
;; (el-get-bundle! breadcrumb)
;; (with-eval-after-load 'breadcrumb
;;   (global-set-key (kbd "C-'") 'bc-set)
;;   (global-set-key (kbd "M-j") 'bc-previous)
;;   (global-set-key (kbd "M-J") 'bc-next)
;;   (global-set-key (kbd "C-c j") 'bc-goto-current)
;;   (global-set-key (kbd "C-c C-j") 'bc-list))


;; whole-line-or-region
(use-package whole-line-or-region
  :ensure t
  :init
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

  (whole-line-or-region-global-mode))


;; VC mode, do not ever annoy me with slow file loading time
(remove-hook 'find-file-hook 'vc-find-file-hook)


;; smerge
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)
(setq smerge-command-prefix (kbd "C-c ]"))


;; (el-get-bundle! multi-mode
;;   :type http
;;   :url "http://www.gerd-neugebauer.de/software/emacs/multi-mode/multi-mode.el")
;; (with-eval-after-load 'multi-mode
;;   (defun better-django-html-mode () (interactive)
;;          (multi-mode 1
;;                      'django-html-mode
;;                      '("<script type=\"text/javascript\">" js-mode)
;;                      '("</script>" django-html-mode)
;;                      '("<style type=\"text/css\">" css-mode)
;;                      '("</style>" django-html-mode)))

;;   (delete '("\\.html\\'" . django-html-mode) auto-mode-alist)
;;   (add-to-list 'auto-mode-alist '("\\.html\\'" . better-django-html-mode)))


(use-package clojure-mode
  :ensure t
  :mode ("\\.boot\\'" . clojure-mode)
        ("\\.edn\\'" . clojure-mode)
  :config
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (setq clojure-indent-style :always-indent)
  (setq clojure-thread-all-but-last t)
  (define-clojure-indent
    (= 0)
    (+ 0)
    (- 0)
    (* 0)
    (->  0)
    (->> 0)
    (and 0)
    (or  0)
    (and* 0)
    (or* 0)
    (recur 0)))


(use-package cider
  :ensure t
  :pin melpa-stable
  :commands cider-mode
  :bind (:map cider-repl-mode-map
         ("C-c M-r" . cider-repl-previous-matching-input)
         ("C-c M-s" . cider-repl-next-matching-input))
  :init
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-cljs-repl "(do (require '[figwheel-sidecar.repl-api :as ra]) (ra/cljs-repl))")
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode))

(use-package clj-refactor
  :ensure t
  :pin melpa-stable)

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode))


(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map
         ("C-<left>" . nil)
         ("C-<right>" . nil)
         ("C-M-," . paredit-forward-barf-sexp)
         ("C-M-." . paredit-forward-slurp-sexp))
  :init
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))


(use-package sql-indent
  :ensure t
  :init
  (with-eval-after-load 'sql
    (require 'sql-indent)))


;; Various

(use-package dockerfile-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-save-repository-buffers nil))

(use-package git-timemachine
  :ensure t
  :bind (("C-x G" . git-timemachine)))

(use-package string-inflection
  :ensure t)

(use-package piu
  :bind (("C-x p" . piu)))

(use-package graphviz-dot-mode
  :ensure t)

(use-package dumb-jump
  :ensure t
  :bind (("C-c g o" . dumb-jump-go-other-window)
         ("C-c g b" . dumb-jump-back)
         ("C-c g j" . dumb-jump-go)
         ("C-c g q" . dumb-jump-quick-look))
  :config
  (setq dumb-jump-selector 'ivy)
  (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljc"))
  (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljs"))
  (add-to-list 'dumb-jump-find-rules
               '(:type "function" :language "clojure"
                       :regex "\\\(rum/defcs?\\s+JJJ\\j")))

(use-package fzf
  :ensure t
  :bind ("C-c o" . fzf)
  :init
  (setenv "FZF_DEFAULT_COMMAND" "fd -t f"))

(use-package restclient
  :ensure t
  :mode ("\\.rest\\'" . restclient-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/unmark-next-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-S-c C-S-c" . mc/edit-lines)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :config
  (setq lua-indent-level 2))

(use-package ialign
  :ensure t)

(use-package vcl-mode
  :ensure t
  :config
  (setq vcl-indent-level 2))

(use-package minions
  :ensure t
  :config (minions-mode 1))

(use-package moody
  :ensure t
  :init
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 18)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; (use-package av-psql
;;   :init
;;   (av-wireup-pg-stuff)
;;   :bind
;;   :map )


(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode))


(use-package string-edit
  :ensure t)


(use-package deft
  :ensure t
  :bind ("C-c C-q" . deft)
  :commands (deft)
  :init
  (setq deft-directory "~/Documents/kb"
        deft-default-extension "md"
        deft-use-filename-as-title t
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase))))
