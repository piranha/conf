;;; modes.el -- various modes configuration

(require 'warnings)

;; usual major modes
(add-to-list 'auto-mode-alist '("\\.egg\\'" . archive-mode))

(require 'imenu)
(setq imenu-auto-rescan t)
(use-package flimenu
  :ensure t
  :config
  (flimenu-global-mode))

(setq tags-file-name (expand-file-name "~/TAGS"))



;; dired
(setq dired-omit-extensions '(".pyc" ".elc")
      dired-omit-files "^\\.?#\\|^\\.")
(autoload 'dired-jump "dired-x" "Jump to dir of current file" t)
(autoload 'dired-omit-mode "dired-x" "Omit unnecessary files in dired view" t)
(add-hook 'dired-mode-hook 'dired-omit-mode)
(add-hook 'dired-omit-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M-o") nil)
            (define-key dired-mode-map (kbd "M-O") 'dired-omit-mode)
            (define-key dired-mode-map (kbd "C-,")
              (lambda () (bs--show-with-configuration "dired")))))
(global-set-key (kbd "C-x C-d") 'dired-jump)

(setq eshell-scroll-to-bottom-on-input 'all
      eshell-error-if-no-glob t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions nil)

(column-number-mode 1)
(show-paren-mode 1)
(global-subword-mode 1)


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

(winner-mode 1) ;; window configuration undo/redo
(remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)
(windmove-default-keybindings)

(setq tramp-default-method "ssh"
      tramp-use-ssh-controlmaster-options nil)
;; (eval-after-load 'tramp-mode
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


;; saving place in file
(save-place-mode 1)

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

;; org mode

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
              ("C-," . nil)
              ("C-S-<up>" . org-timestamp-up)
              ("C-S-<down>" . org-timestamp-down))
  :config
  (setq org-hide-leading-stars t))


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
  :mode "\\.go\\'"
  :commands godoc-gogetdoc
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'whitespace-style)
                   '(face trailing lines-tail))
              (whitespace-mode -1)
              (whitespace-mode 1)))
  :custom
  (godoc-at-point-function #'godoc-gogetdoc))

(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")

(use-package sass-mode
  :ensure t
  :mode "\\.scss")

(use-package less-css-mode
  :ensure t
  :mode "\\.less")

(setq css-indent-offset 2)

(use-package po-mode
  :ensure t
  :mode "\\.po\\'")

;;;;;;;;;
;; Python
;;;;;;;;;

(defvar python-mode-map)
(add-hook 'python-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'python-imenu-create-flat-index)
            (define-key python-mode-map (kbd "RET") 'newline-maybe-indent)))


(use-package flycheck-pos-tip
  :ensure t
  :commands flycheck-pos-tip-error-messages)


(use-package flycheck
  :ensure t
  :commands global-flycheck-mode
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq-default flycheck-display-errors-function 'flycheck-pos-tip-error-messages)
  ;(add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint)
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp)
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
  (add-to-list 'flycheck-disabled-checkers 'scss-lint)
  (add-to-list 'flycheck-disabled-checkers 'sass/scss-sass-list))


(use-package flycheck-pyflakes
  :ensure t
  ;; :hook python-mode-hook
  :init
  (add-hook 'python-mode-hook (lambda () (require 'flycheck-pyflakes))))


(use-package flycheck-clj-kondo
  :ensure t
  :defer t)


;; Ruby

(defvar ruby-mode-map)
(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent))

;; Javascript

(defvar js-mode-map)
(with-eval-after-load 'js
  (setq js-indent-level 2)
  (define-key js-mode-map (kbd "RET") 'newline-maybe-indent))

(defun imenu-dumb-js-make-index ()
  (save-excursion
    (imenu--generic-function '((nil "function\\s-+\\([^ ]+\\)(" 1)
                               (nil "\\.\\([^\\. ]+\\)\\s-*=\\s-*function\\s-*(" 1)))))

(add-hook 'js-mode-hook
          #'(lambda ()
              (setq imenu-create-index-function 'imenu-dumb-js-make-index)))


;; Snippets

(use-package yasnippet
  :ensure t
  :commands yas-global-mode
  :bind (:map yas-minor-mode-map
         ("C-/" . yas-expand)
         ("<tab>" . nil))
  :init
  (add-hook 'snippet-mode-hook
            #'(lambda ()
                (set (make-local-variable 'require-final-newline) nil)))
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/"))


(use-package yasnippet-snippets
  :ensure t)


;; highlight parentheses
(use-package highlight-parentheses
  :ensure t
  :init
  (dolist (hook '(python-mode-hook
                  emacs-lisp-mode-hook
                  clojure-mode-hook))
    (add-hook hook 'highlight-parentheses-mode)))


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
  :commands
  whole-line-or-region-global-mode
  whole-line-or-region-call-with-region
  :bind (("C-w" . whole-line-kill-region-or-word-backward))
  :init
  (defun whole-line-kill-region-or-word-backward (prefix)
    "Kill (cut) region or just a single word backward"
    (interactive "*p")
    (if (whole-line-or-region-use-region-p)
        (kill-region (region-beginning) (region-end) 'region)
      (if (bound-and-true-p paredit-mode)
          (paredit-backward-kill-word)
        (subword-backward-kill prefix))))

  (setq whole-line-or-region-extensions-alist
        '((copy-region-as-kill whole-line-or-region-copy-region-as-kill nil)
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


(use-package clojure-mode
  :ensure t
  :defer t
  :commands clojure-mode put-clojure-indent
  :mode ("\\.boot\\'" . clojure-mode)
        ("\\.edn\\'" . clojure-mode)
        ("\\.bb\\'" . clojure-mode)
  :config
  (require 'flycheck-clj-kondo)
  (setq clojure-indent-style :always-indent)
  (setq clojure-thread-all-but-last t)
  (setq clojure-align-forms-automatically t)
  (setq clojure-toplevel-inside-comment-form t)
  (put-clojure-indent '= 0)
  (put-clojure-indent 'not= 0)
  (put-clojure-indent '+ 0)
  (put-clojure-indent '- 0)
  (put-clojure-indent '* 0)
  (put-clojure-indent '/ 0)
  (put-clojure-indent '> 0)
  (put-clojure-indent '< 0)
  (put-clojure-indent '>= 0)
  (put-clojure-indent '<= 0)
  (put-clojure-indent '->  0)
  (put-clojure-indent '->> 0)
  (put-clojure-indent 'and 0)
  (put-clojure-indent 'or  0)
  (put-clojure-indent 'and* 0)
  (put-clojure-indent 'or* 0)
  (put-clojure-indent 'recur 0)
  (add-to-list 'clojure-align-cond-forms "better-cond.core/when-let")
  (add-to-list 'clojure-align-cond-forms "better-cond.core/if-let")
  (add-to-list 'clojure-align-binding-forms "blet"))


(use-package cider
  :pin melpa-stable
  :ensure t
  ;; :defer t
  :commands cider-mode
  :bind (:map cider-mode-map
              ("C-c C-f" . nil)
        :map cider-repl-mode-map
              ("C-c M-r" . cider-repl-previous-matching-input)
              ("C-c M-s" . cider-repl-next-matching-input))
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  :config
  (setq cider-repl-history-file "~/.emacs.d/cider-history"
        cider-cljs-repl "(do (require '[figwheel-sidecar.repl-api :as ra]) (ra/cljs-repl))"
        cider-repl-display-help-banner nil)
  (add-to-list 'warning-suppress-types '(undo discard-info)))

(use-package paredit
  ;;  :ensure t ;; it's in packages/paredit.el
  :no-require t
  :commands paredit-mode
  :defines paredit-mode-map
  :bind (:map paredit-mode-map
              ("C-<left>" . nil)
              ("C-<right>" . nil)
              ("C-M-," . paredit-forward-barf-sexp)
              ("C-M-." . paredit-forward-slurp-sexp))
  :init
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-to-list 'package--builtin-versions `(paredit 26)))

(use-package clj-refactor
  :pin melpa-stable
  :ensure t
  :load-path "elpa/clj-refactor-20230202.637/"
  :commands (clj-refactor-mode cljr-add-keybindings-with-prefix)
  :init
  (add-hook 'clojure-mode-hook
            #'(lambda ()
                (clj-refactor-mode 1)
                (cljr-add-keybindings-with-prefix "C-c C-m"))))


(use-package sql-indent
  :ensure t
  :commands sqlind-minor-mode
  :init
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))


(use-package av-psql
  :commands av-pg-server-connect
  :config
  (av-wireup-pg-stuff))


;; Various

(use-package dockerfile-mode
  :ensure t
  :defer t
  :commands dockerfile-mode
  :mode "Dockerfile\\'")

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (setq magit-save-repository-buffers nil)

  ;;; https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (add-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)

  (defun magit-rebase-origin-master (args)
    (interactive (list (magit-rebase-arguments)))
    (message "Rebasing...")
    (magit-git-rebase "origin/master" args)
    (message "Rebasing...done"))

  (transient-define-suffix magit-push-current-with-lease (args)
    "Push the current branch to its push-remote with lease."
    :if 'magit-get-current-branch
    :description 'magit-push--pushbranch-description
    (interactive (list (magit-push-arguments)))
    (pcase-let ((`(,branch ,remote)
                 (magit--select-push-remote "push there")))
      (run-hooks 'magit-credential-hook)
      (magit-run-git-async "push" "-v" "--force-with-lease" args remote
                           (format "refs/heads/%s:refs/heads/%s"
                                   branch branch))))

  (transient-append-suffix 'magit-rebase "e" '("o" "origin/master" magit-rebase-origin-master))
  (transient-append-suffix 'magit-push "e" '("P" "pushRemote with lease" magit-push-current-with-lease)))

(use-package string-inflection
  :ensure t)

(use-package piu
  :bind (("C-x p" . piu)))

(use-package graphviz-dot-mode
  :ensure t
  :mode "\\.dot\\'")

(use-package dumb-jump
  :ensure t
  :commands dumb-jump-xref-activate
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljc"))
  (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljs"))
  (add-to-list 'dumb-jump-find-rules
               '(:type "function" :language "clojure"
                       :regex "\\\(rum/defcs?\\s+JJJ\\j")))

(use-package fzf
  :ensure t
  :bind ("C-c o" . fzf)
  :config
  (setenv "FZF_DEFAULT_COMMAND" "fd -t f"))

(use-package restclient
  :ensure t
  :mode ("\\.rest\\'" . restclient-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/unmark-next-like-this)
         ("C-c C-." . mc/mark-all-dwim)
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
  :mode "\\.vcl\\'"
  :config
  (setq vcl-indent-level 2))

(use-package minions
  :ensure t
  :init (minions-mode 1))

(use-package hl-todo
  :ensure t
  :init (global-hl-todo-mode)
  :config
  ;;(set-face-foreground 'hl-todo "#ff0000")
  (add-to-list 'hl-todo-keyword-faces '("TODO" . "#863F3F"))
  (add-to-list 'hl-todo-keyword-faces '("FIXME" . "#e18a07"))
  (add-to-list 'hl-todo-keyword-faces '("NOTE" . "#08632C"))
  (add-to-list 'hl-todo-keyword-faces '("DONE" . "#08632C")))


(use-package string-edit
  :ensure t)


(use-package deft
  :ensure t
  :bind ("C-c ]" . deft)
  :commands (deft deft-refresh)
  :init
  (setq deft-directory "~/Documents/kb"
        deft-default-extension "md"
        deft-use-filename-as-title t
        deft-recursive t
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase)))
  (defconst blog-buffer "*Blog*" "Blog buffer name.")
  :config
  (defun deft-current-window-width ()
    (let ((window (get-buffer-window deft-buffer)))
      (when window
        (- (window-text-width window) 1))))
  ;;; (defun new-post ())
  (defun blog ()
    (interactive)
    (let ((deft-directory (expand-file-name "~/dev/web/solovyov.net/src/blog")))
      (if (get-buffer deft-buffer)
          (and (switch-to-buffer deft-buffer)
               (deft-refresh))
        (deft)))))


;; (use-package anzu
;;   :ensure t
;;   :commands
;;   global-anzu-mode
;;   anzu-query-replace
;;   anzu-query-replace-at-cursor
;;   anzu-isearch-query-replace
;;   anzu-isearch-query-replace-regexp
;;   :bind (([remap query-replace] . #'anzu-query-replace)
;;          ("C-:" . #'anzu-query-replace-at-cursor)
;;          :map isearch-mode-map
;;          ([remap isearch-query-replace] . #'anzu-isearch-query-replace)
;;          ([remap isearch-query-replace-regexp] . #'anzu-isearch-query-replace-regexp))
;;   :init (global-anzu-mode 1))


(use-package iflipb
  :ensure t
  :bind (("M-]" . iflipb-next-buffer)
         ("M-[" . iflipb-previous-buffer)))


(use-package writeroom-mode
  :ensure t
  :bind (:map writeroom-mode-map
              ("C-M-<" . writeroom-decrease-width)
              ("C-M->" . writeroom-increase-width)
              ("C-M-=" . writeroom-adjust-width))
  :init
  (setq writeroom-width 80)
  (add-hook 'writeroom-mode-hook
            #'(lambda ()
                (set-face-attribute 'markdown-pre-face (selected-frame) :family "Monaco" :height 140)
                (face-remap-add-relative 'default '(:family "Inter" :height 120))
                (face-remap-add-relative 'cursor '(:background 'red)))))

(use-package terraform-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode "\\.html\\'" "\\.tsx\\'"
  :bind (:map web-mode-map
              ("C-c /" . web-mode-element-close))
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (add-hook 'web-mode-hook
            #'(lambda ()
                (when (and (projectile-project-p)
                           (file-exists-p (concat (projectile-project-root) "manage.py")))
                  (web-mode-set-engine "django")))))


;; mini-frame shows no lines on start when mini-frame-resize is enabled
(setq resize-mini-frames nil)
(setq mini-frame-resize nil)
(use-package mini-frame
  :ensure t
  :config
  (setq mini-frame-show-parameters
        '((left . 0.5)
          (top . 0.3)
          (width . 0.7)
          (height . 10)))
  (add-to-list 'mini-frame-ignore-commands 'ctrlf-forward-literal)
  (add-to-list 'mini-frame-ignore-commands 'ctrlf-forward-fuzzy)
  (add-to-list 'mini-frame-ignore-commands 'ctrlf-forward-regexp)
  (add-to-list 'mini-frame-ignore-commands 'ctrlf-forward-fuzzy-regexp)
  (add-to-list 'mini-frame-ignore-commands 'ctrlf-backward-literal)
  (add-to-list 'mini-frame-ignore-commands 'ctrlf-backward-fuzzy)
  (add-to-list 'mini-frame-ignore-commands 'ctrlf-backward-regexp)
  (add-to-list 'mini-frame-ignore-commands 'ctrlf-backward-fuzzy-regexp)
  (mini-frame-mode 1))


(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'")

(use-package point-stack
  :ensure t
  :bind (("C-M-;" . point-stack-pop)
         ("C-M-'" . point-stack-forward-stack-pop))
  :commands point-stack-setup-advices
  :init
  (point-stack-setup-advices))


(use-package embark
  :ensure t
  :bind
  (("C-'"   . embark-act)
   ("C-\""  . embark-dwim)
   ("C-h B" . embark-bindings)))


(use-package pyvenv
  :ensure t
  :commands (pyvenv-activate pyvenv-workon))

(use-package jinja2-mode
  :ensure t
  :defer t)

(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'")

(use-package typescript-mode
  :ensure t
  :defer t
  :config
  (setq typescript-indent-level 2))

(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("C-M-l" . nil))
  :commands vterm)
