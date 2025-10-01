;;; modes.el -- various modes configuration

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (dolist (var '("LANG"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

(require 'imenu)
(setq imenu-auto-rescan t)
(use-package flimenu ; flatten imenu
  :ensure t
  :config
  (flimenu-global-mode))

(column-number-mode 1)
(show-paren-mode 1)
(setq show-paren-context-when-offscreen 'child-frame)
(global-subword-mode 1)

;;; dired
(setq dired-omit-extensions '(".pyc" ".elc")
      dired-omit-files "^\\.?#\\|^\\.")
(autoload 'dired-jump "dired-x" "Jump to dir of current file" t)
(autoload 'dired-omit-mode "dired-x" "Omit unnecessary files in dired view" t)
(add-hook 'dired-mode-hook 'dired-omit-mode)
(add-hook 'dired-omit-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M-o") nil)
            (define-key dired-mode-map (kbd "M-O") 'dired-omit-mode)))

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

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package framemove
  :config
  (setq framemove-hook-into-windmove t))

(setq tramp-default-method "ssh"
      tramp-use-ssh-controlmaster-options nil)
;; (eval-after-load 'tramp-mode
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


;; saving place in file
(save-place-mode 1)

;; saving history
;; (savehist-mode 1)
;; (setq savehist-additional-variables
;;       '(search-ring
;;         regexp-search-ring
;;         kill-ring
;;         mark-ring
;;         global-mark-ring
;;         file-name-history
;;         command-history
;;         shell-command-history))
(setq save-abbrevs nil)

(use-package desktop
  :config
  (setq desktop-restore-frames nil
        desktop-files-not-to-save ".*"
        desktop-buffers-not-to-save ".*")
  (add-to-list 'desktop-globals-to-save 'command-history)
  (add-to-list 'desktop-globals-to-save 'shell-command-history)
  (add-to-list 'desktop-globals-to-save 'kill-ring)
  (add-to-list 'desktop-globals-to-save 'global-mark-ring)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookuqp-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  :init
  (desktop-save-mode 1))

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
  (setq org-hide-leading-stars nil)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'visual-fill-column-mode)
  (setq visual-fill-column-width 100))


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
  :mode "\\.yaml\\'"
  :init
  (add-hook 'yaml-mode-hook #'(lambda () (setq-local flycheck-check-syntax-automatically '(save mode-enabled)))))

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

(use-package python-pytest
  :ensure t
  :bind (:map python-mode-map
              ("C-c C-t" . python-pytest-dispatch)))


;;; Flycheck

(use-package flycheck
  :ensure t
  :commands global-flycheck-mode
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 0)
  (setq flycheck-idle-change-delay 1)
  ;(add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint)
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp)
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
  (add-to-list 'flycheck-disabled-checkers 'scss-lint)
  (add-to-list 'flycheck-disabled-checkers 'sass/scss-sass-list))

(use-package flycheck-clj-kondo
  :ensure t
  :defer t)

(use-package flycheck-biomejs
  :ensure t
  :vc (flycheck-biomejs :url "https://github.com/craneduck/flycheck-biomejs"))

(use-package sideline
  :ensure t
  :config
  (setq sideline-backends-right '(sideline-flycheck)
        sideline-backends-right-skip-current-line nil))

(use-package sideline-flycheck
  :ensure t
  :hook (flycheck-mode . sideline-mode))

;; Ruby

(defvar ruby-mode-map)
(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent))

;; Javascript

(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . js-json-mode))
(add-to-list 'auto-mode-alist '("\\.jtd\\'" . js-json-mode))

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
              (setq imenu-create-index-function 'imenu-dumb-js-make-index)
              (setq-local outline-regexp "///")))

(add-hook 'typescript-mode-hook
          #'(lambda ()
              (setq imenu-create-index-function 'imenu-dumb-js-make-index)
              (setq-local outline-regexp "///")))

(use-package json-mode
  :ensure t)

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

;;; VC mode, do not ever annoy me with slow file loading time
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
  (put-clojure-indent 'and 0)
  (put-clojure-indent 'or 0)
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
  (put-clojure-indent '<< 0)
  (add-to-list 'clojure-align-cond-forms "better-cond.core/when-let")
  (add-to-list 'clojure-align-cond-forms "better-cond.core/if-let")
  (add-to-list 'clojure-align-cond-forms "core/cond+")
  (add-to-list 'clojure-align-cond-forms "core/cx")
  (add-to-list 'clojure-align-binding-forms "blet")
  (add-to-list 'clojure-align-binding-forms "mt/with-dynamic-redefs")
  (add-to-list 'clojure-align-binding-forms "with-yielding")

  (defun clojure-match-next-def ()
  "Scans the buffer backwards for the next \"top-level\" definition.
Called by `imenu--generic-function'."
  ;; we have to take into account namespace-definition forms
  ;; e.g. s/defn
  (when (re-search-backward "^[ \t]*(\\([a-z0-9.-]+/\\)?\\(def[^ \n\t]*\\) " nil t)
    (save-excursion
      (let (found?
            (deftype (match-string 2))
            (start (point)))
        ;; ignore user-error from down-list when called from inside a string or comment
        ;; TODO: a better workaround would be to wrap it in
        ;; unless (ppss-comment-or-string-start (syntax-ppss)) instead of ignore-errors,
        ;; but ppss-comment-or-string-start is only available since Emacs 27
        (ignore-errors
          (down-list))
        (forward-sexp)
        (while (not found?)
          (ignore-errors
            (forward-sexp))
          (or (when (char-equal ?\[ (char-after (point)))
                (backward-sexp))
              (when (char-equal ?\) (char-after (point)))
                (backward-sexp)))
          (cl-destructuring-bind (def-beg . def-end) (bounds-of-thing-at-point 'sexp)
            (when (char-equal ?^ (char-after def-beg))
              ;; move to the beginning of next sexp
              (progn (forward-sexp) (backward-sexp)))
            (when (or (not (char-equal ?^ (char-after def-beg)))
                      (and (char-equal ?^ (char-after (point))) (= def-beg (point))))
              (setq found? t)
              (when (string= deftype "defmethod")
                (setq def-end (progn (goto-char def-end)
                                     (forward-sexp)
                                     (point))))
              (when (or (string= deftype "defendpoint")
                        (string= deftype "defendpoint-async"))
                (setq def-end (progn (goto-char def-end)
                                     (forward-sexp)
                                     (point))))
              (set-match-data (list def-beg def-end)))))
        (goto-char start))))))

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
  :hook clojure-mode
  :config
  (setq cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-display-help-banner nil
        cider-download-java-sources t)
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
              ("C-M-." . paredit-forward-slurp-sexp)
              ("C-M-'" . paredit-convolute-sexp))
  :hook clojure-mode
  :hook inf-clojure-mode
  :hook cider-repl-mode
  :hook emacs-lisp-mode
  :hook lisp-data-mode
  :init
  (add-to-list 'package--builtin-versions `(paredit 26)))

(use-package clj-refactor
  :pin melpa-stable
  :ensure t
  :load-path "elpa/clj-refactor-20230202.637/"
  :commands (clj-refactor-mode cljr-add-keybindings-with-prefix)
  :hook clojure-mode
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (setq cljr-insert-newline-after-require nil)
  (defun cljr--unresolved-alias-ref--preventer (alias-ref)
    (unless (or (string= "user" alias-ref)
                (string= "dev" alias-ref)
                (string-prefix-p "dev." alias-ref))
      alias-ref))

  (advice-add 'cljr--unresolved-alias-ref :before-while
              #'cljr--unresolved-alias-ref--preventer))

;; (use-package eglot
;;   :ensure t
;;   :bind (("C-c a" . eglot-code-actions))
;;   :hook (((clojure-mode clojurec-mode clojurescript-mode java-mode scala-mode)
;;           . eglot-ensure)
;;          ((cider-mode eglot-managed-mode) . eglot-disable-in-cider))
;;   :preface
;;   (defun eglot-disable-in-cider ()
;;     (when (eglot-managed-p)
;;       (if (bound-and-true-p cider-mode)
;;           (progn
;;             (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t)
;;             (remove-hook 'xref-backend-functions 'eglot-xref-backend t))
;;         (add-hook 'completion-at-point-functions 'eglot-completion-at-point nil t)
;;         (add-hook 'xref-backend-functions 'eglot-xref-backend nil t))))
;;   :custom
;;   (eglot-confirm-server-initiated-edits nil)
;;   (eglot-sync-connect nil)
;;   (eglot-connect-timeout 120)
;;   (eglot-autoshutdown t)
;;   (eglot-events-buffer-size 0)
;;   (eglot-extend-to-xref nil)
;;   (eglot-ignored-server-capabilities
;;    '(:hoverProvider
;;      :documentHighlightProvider
;;      :documentFormattingProvider
;;      :documentRangeFormattingProvider
;;      :documentOnTypeFormattingProvider
;;      :colorProvider
;;      :foldingRangeProvider))
;;   (eglot-stay-out-of '(yasnippet)))

;; Various

(use-package sql-indent
  :ensure t
  :commands sqlind-minor-mode
  :init
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))

(use-package av-psql
  :commands av-pg-server-connect
  :config
  (av-wireup-pg-stuff))

;; like cider-repl, c-o cleans last output, c-u c-o cleans full buffer
(defadvice comint-delete-output (around partial compile activate)
  (pcase-exhaustive current-prefix-arg
    ('nil ad-do-it)
    ('(4) (comint-clear-buffer))))

(ad-activate 'comint-delete-output)

(use-package dockerfile-mode
  :ensure t
  :defer t
  :commands dockerfile-mode
  :mode "Dockerfile\\'")

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :hook (magit-process-mode . goto-address-mode)
  :config
  (setq magit-save-repository-buffers nil
        magit-log-section-commit-count 20)

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

  (defun magit-pull-origin-master (args)
    (interactive (list (magit-fetch-arguments)))
    (if-let ((main (-> (magit-git-string-p "symbolic-ref" "refs/remotes/origin/HEAD")
                       (split-string "/")
                       (last)
                       (car))))
        (progn
         (message "Fetching origin:%s..." main)
         (magit-git-fetch "origin" (concat main ":" main)))
      (message "Cannot determine `main` branch!")))

  (transient-append-suffix 'magit-rebase "e" '("M" "origin/master" magit-rebase-origin-master))
  (transient-append-suffix 'magit-push "e" '("P" "pushRemote with lease" magit-push-current-with-lease))
  (transient-append-suffix 'magit-pull "e" '("M" "update `main` branch from origin" magit-pull-origin-master)))

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
         ("C-x C-." . mc/mark-all-dwim)
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


(use-package terraform-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode "\\.html\\'" "\\.tsx\\'" "\\.tmpl\\'"
  :bind (:map web-mode-map
              ("C-c /" . web-mode-element-close))
  :init
  (setq web-mode-enable-engine-detection t)
  (setq web-mode-enable-auto-indentation nil)
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
  (mini-frame-mode 1))


(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'")

(use-package pyvenv
  :ensure t
  :commands (pyvenv-activate pyvenv-workon))

(use-package jinja2-mode
  :ensure t
  :defer t
  :bind (:map jinja2-mode-map
              ("M-o" . nil)))

(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'")

(use-package typescript-mode
  :ensure t
  :defer t
  :config
  (setq typescript-indent-level 2))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package goto-last-change
  :ensure t
  :bind ("C-x C-\\" . goto-last-change))

(use-package point-stack
  :ensure t
  :bind (("C-M-j" . point-stack-pop)
         ("C-M-k" . point-stack-forward-stack-pop)
         :map paredit-mode-map
         ("C-M-k" . nil))
  :config
  (point-stack-setup-advices))

;; https://github.com/shouya/ancilla.el

(use-package git-link
  :ensure t)

(use-package treesit-langs
  :ensure t
  :config
  (treesit-langs-major-mode-setup))

(use-package highlight-indent-guides
  :ensure t
  :hook yaml-mode
  :config
  (setq highlight-indent-guides-method 'character))

(use-package explain-pause-mode)

;;; janet

(use-package janet-mode
  :ensure t
  :init
  (add-hook 'janet-mode-hook 'paredit-mode))

(use-package flycheck-janet
  :ensure t)

(use-package pkl-mode
  :ensure t
  :config
  (setq pkl-enable-copilot nil))

(use-package significant-other
  :ensure t
  :vc (significant-other :url "https://github.com/ovistoica/significant-other.el")
  :commands with-significant-others significant-other-jump
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (with-significant-others
               file
               ("/src/.+\\.cljc?$"
                (list (->> file
                           (replace-regexp-in-string "\\.\\(cljc?\\)$" "_test.\\1")
                           (replace-regexp-in-string "/src/" "/test/"))))

               ("/test/.+_test\\.cljc?$"
                (list (->> file
                           (replace-regexp-in-string "_test\\.\\(cljc?\\)$" ".\\1")
                           (replace-regexp-in-string "/test/" "/src/"))))))))

;;; llms

(use-package eat ;; terminal
  :ensure t)

(use-package gptel
  :ensure t
  :mode ("\\.llm\\'" . org-mode)
  :bind (:map gptel-mode-map
              ("C-c C-c" . gptel-send))
  :config
  (setq gptel-model 'claude-sonnet-4-20250514
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t :key (cdr (netrc "api.anthropic.com")))
        ;; gptel-backend (gptel-make-openai "gpt"
        ;;                 :stream t :key (cdr (netrc "api.openai.com"))
        ;;                 :models '(o1-preview gpt-4o))
        ;; gptel-backend (gptel-make-gemini "gemini"
        ;;                 :stream t :key (cdr (netrc "generativelanguage.googleapis.com"))
        ;;                 :models '(gemini-2.0-flash gemini-2.0-flash-lite))
        gptel-default-mode 'org-mode
        gptel-org-branching-context t)
  (add-hook 'org-mode-hook #'(lambda ()
                               (when (and buffer-file-name
                                          (string-match-p "\\.llm[^.]*$" buffer-file-name))
                                 (gptel-mode 1))))

  (defun gptel-set-default-directory ()
    (unless (buffer-file-name)
      (setq default-directory "~/dev/misc/llmchats/")))
  (add-hook 'gptel-mode-hook #'gptel-set-default-directory)

  (defun gptel-rename-chat ()
    (interactive)
    (unless gptel-mode
      (user-error "This command is intended to be used in gptel chat buffers."))
    (gptel-request
        (list nil                         ;user
              "What is the chat content?" ;llm
              (concat "```" (if (eq major-mode 'org-mode) "org" "markdown") "\n"
                      (buffer-substring-no-properties (point-min) (point-max))
                      "\n```"))         ;user
      :system
      (list (format                     ;system message
             "I will provide a transcript of a chat with an LLM.  \
Suggest a short and informative name for a file to store this chat in.  \
Use the following guidelines:
- be very concise, one very short sentence at most
- no spaces, use underscores if required
- return ONLY the title, no explanation or summary
- append the extension .%s"
             (if (eq major-mode 'org-mode) "org" "md")))
      :callback
      (lambda (resp info)        ;callback called with response and request info
        (if (stringp resp)
            (let ((buf (plist-get info :buffer)))
              (when (and (buffer-live-p buf)
                         (y-or-n-p (format "Rename buffer %s to %s? " (buffer-name buf) resp)))
                (with-current-buffer buf (rename-visited-file resp))))
          (message "Error(%s): did not receive a response from the LLM."
                   (plist-get info :status)))))))

(use-package aidermacs
  :ensure t
  :vc (aidermacs :url "https://github.com/MatthewZMD/aidermacs")
  :bind ("C-c a" . aidermacs-transient-menu)
  :config
  (setq aidermacs-default-model "gemini" ;;"sonnet"
        ;;"gemini/gemini-2.5-pro-exp-03-25"
        ;;"anthropic/claude-3-7-sonnet-20250219"
        aidermacs-backend 'eat)
  (setenv "ANTHROPIC_API_KEY" (cdr (netrc "api.anthropic.com")))
  (setenv "GEMINI_API_KEY" (cdr (netrc "generativelanguage.googleapis.com"))))

(use-package claude-code-ide
  :ensure t
  :vc (claude-code-ide :url "https://github.com/manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'eat))

(use-package eca
  :ensure t
  :commands (eca)
  :config
  (setenv "ANTHROPIC_API_KEY" (cdr (netrc "api.anthropic.com"))))

;;; modes-init.el ends here
