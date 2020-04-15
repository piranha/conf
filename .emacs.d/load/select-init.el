(use-package ctrlf
  :git "https://github.com/raxod502/ctrlf"
  :bind (:map ctrlf--keymap
         ([remap isearch-forward ] . ctrlf-forward-fuzzy)
         ([remap isearch-backward] . ctrlf-backward-fuzzy))
  :config
  (ctrlf-mode 1))

(use-package selectrum
  :git "https://github.com/raxod502/selectrum"
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :git (prescient :url "https://github.com/raxod502/prescient.el"
                  :files ("selectrum-prescient.el"))
  :config
  (selectrum-prescient-mode)
  (prescient-persist-mode))


(use-package deadgrep
  :ensure t
  :commands (deadgrep--read-search-term)
  :bind (("C-c s" . deadgrep)))


(defun projectile-selection-at-point ()
  (when (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun projectile-deadgrep (search-term)
  (interactive (list (deadgrep--read-search-term)))
  (let ((deadgrep-project-root-function #'projectile-project-root))
    (deadgrep search-term)))

(use-package projectile
  :ensure t
  :commands projectile-mode
  :bind (("M-t" . projectile-find-file)
         ("C-c p" . projectile-command-map)
         :map projectile-command-map
         ("s s" . projectile-deadgrep))
  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'default)
  (projectile-mode 1))


;;; selection-at-point

(defun citre-completion-in-region (start end collection &optional predicate)
  "A function that replace default `completion-in-region-function'.
This completes the text between START and END using COLLECTION.
PREDICATE says when to exit.

When there are multiple candidates, this uses standard
`completing-read' interface, while the default one in Emacs pops
a *Completions* buffer to show them.  When combined with some
minibuffer completion framework, it's more user-friendly then the
default one.

Notice when `completing-read-function' is
`completing-read-default' (i.e., not enhanced by a minibuffer
completion framework), this falls back to
`completion--in-region'."
  (if (eq completing-read-function #'completing-read-default)
      (completion--in-region start end collection predicate)
    (let* ((str (buffer-substring-no-properties start end))
           (completion-ignore-case (string= str (downcase str)))
           (candidates
            (nconc
             (completion-all-completions str collection predicate (- end start))
             nil))
           (completion nil))
      (pcase (length candidates)
        (0 (message "No completions"))
        (1 (setq completion (car candidates)))
        (_ (setq completion (completing-read (format "(%s): " str)
                                             candidates predicate t))))
      (when completion
        (delete-region start end)
        (insert (substring-no-properties completion))))))

(setq completion-in-region-function #'citre-completion-in-region)


;;; recentf

(defun as/recentf-open-files ()
  (interactive)
  (find-file (selectrum-read "Find recent file: " recentf-list)))

(use-package recentf-mode
  :bind ("C-c C-x C-f" . as/recentf-open-files)
  :config
  (setq recentf-max-menu-items 200
        recentf-max-saved-items 200)
  :init
  (recentf-mode 1))


;; (use-package counsel
;;   :ensure t
;;   :bind (;;("C-." . counsel-ibuffer)
;;          ("C-." . ivy-switch-buffer)
;;          ("C-c v" . ivy-push-view)
;;          ("C-c V" . ivy-pop-view)
;;          ("C-c r" . ivy-resume)
;;          ("M-x" . counsel-M-x)
;;          ("M-A" . counsel-imenu)
;;          ("C-x C-f" . counsel-find-file)
;;          ("C-c C-x C-f" . counsel-recentf)
;;          ("C-h f" . counsel-describe-function)
;;          ("C-h v" . counsel-describe-variable)
;;          ("C-c j" . counsel-git-grep)
;;          ("C-c s" . counsel-rg)
;;          ("C-c C-y" . counsel-yank-pop)
;;          ("C-c C-i" . counsel-mark-ring)
;;          ;;("C-c o" . counsel-fzf)
;;          :map read-expression-map
;;          ("C-r" . counsel-minibuffer-history))
;;   :init
;;   (setq ivy-use-virtual-buffers t
;;         ivy-use-selectable-prompt t
;;         ivy-display-style 'fancy)
;;   (setenv "FZF_DEFAULT_COMMAND" "fd -t f")
;;   (ivy-mode 1))

;; (use-package swiper
;;   :ensure t
;;   :bind ("C-s" . swiper))

;; (use-package imenu-anywhere
;;   :ensure t
;;   :commands ivy-imenu-anywhere
;;   :bind ("C-c M-A" . ivy-imenu-anywhere))

;; (use-package counsel-projectile
;;   :ensure t
;;   :commands counsel-projectile-mode
;;   :init
;;   (counsel-projectile-mode))

;; (use-package ivy-prescient
;;   :ensure t
;;   :commands ivy-prescient-mode
;;   :init
;;   (ivy-prescient-mode)
;;   (prescient-persist-mode)
;;   (setq ivy-initial-inputs-alist '()))

;; (use-package ivy-posframe
;;   :ensure t
;;   :commands
;;   ivy-posframe-display-at-frame-bottom-left
;;   ivy-posframe-enable
;;   :init
;;   (setq ivy-posframe-width (frame-width))
;;   (setq ivy-posframe-hide-minibuffer t)
;;   (setq ivy-posframe-parameters
;;         '((left-fringe . 0)
;;           (right-fringe . 0)))
;;   (add-to-list 'ivy-display-functions-alist
;;                '(t . ivy-posframe-display-at-frame-bottom-left))
;;   (ivy-posframe-enable))
