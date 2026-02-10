;; -*- lexical-binding: t; -*-

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

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
  (add-to-list 'mini-frame-ignore-commands 'isl-search)
  (add-to-list 'mini-frame-ignore-commands 'isl-resume)
  (mini-frame-mode 1))

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode 1))

(use-package orderless
  :ensure t
  :custom
  ;; https://github.com/oantolin/orderless?tab=readme-ov-file#component-matching-styles
  (orderless-matching-styles '(orderless-literal-prefix orderless-literal orderless-prefixes))
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)
  :config
  (add-to-list 'orderless-affix-dispatch-alist
               '(?~ . orderless-regexp)))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

;;; Grep

(defun deadgrep-here (search-term)
  "Start deadgrep from the current working directory with SEARCH-TERM."
  (interactive (list (deadgrep--read-search-term)))
  (deadgrep search-term default-directory))

(use-package deadgrep
  :ensure t
  :commands (deadgrep--read-search-term)
  :bind (("C-c s" . deadgrep-here)))


(defun projectile-deadgrep (search-term)
  "Deadgrep the whole project for SEARCH-TERM."
  (interactive (list (deadgrep--read-search-term)))
  (let ((deadgrep-project-root-function #'projectile-project-root))
    (deadgrep search-term)))


;;; Projects

(use-package projectile
  :ensure t
  :commands projectile-mode
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map)
         :map projectile-command-map
         ("s" . projectile-deadgrep))
  :init
  (projectile-mode 1)
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'default
        projectile-create-missing-test-files t))


;;; recentf

(defun as/recentf-open-files ()
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list)))

(use-package recentf-mode
  :bind ("C-c C-x C-f" . as/recentf-open-files)
  :init
  (setq as/recentf-timer (run-at-time "5 min" (* 5 60) 'recentf-save-list)
        recentf-max-menu-items 500
        recentf-max-saved-items 500
        ;; this stops loading tramp
        ;; https://stackoverflow.com/questions/880625
        recentf-auto-cleanup 'never)
  (recentf-mode 1))

;;; main navigation

(use-package consult
  :ensure t
  :bind (("C-." . consult-buffer)
         ("C-s" . consult-line)
         ("C-c M-k" . consult-kmacro)
         ("M-y" . consult-yank-pop)
         ("C-x r b" . consult-bookmark) ;; bookmark-jump
         ("M-g M-g" . consult-goto-line)
         ("M-g M-m" . consult-mark)
         ("M-g m" . consult-global-mark)
         ("C-," . consult-global-mark)
         ("M-i" . consult-imenu)
         ("M-g i" . consult-imenu-multi)
         ("M-g o" . consult-outline)
         (:map projectile-mode-map
               ("C-c p b" . consult-project-buffer)))
  :config
  (setq consult-project-function (lambda (_) (projectile-project-root))
        consult-narrow-key "<")

  (setq consult-buffer-sources
        '(consult-source-hidden-buffer
          consult-source-modified-buffer
          consult-source-buffer
          consult-source-project-buffer
          consult-source-project-recent-file
          consult-source-recent-file
          consult-source-file-register
          consult-source-bookmark))

  ;; manual preview for buffer-switching
  (consult-customize consult-buffer :preview-key "M-."))

(use-package consult-projectile
  :ensure t
  :bind (("M-t" . consult-projectile)))

;;; mark navigation

(use-package nav-flash ;; back-button uses this to highlight line after navigation
  :ensure t)

(use-package back-button ;; C-x left/right, C-x C-left/C-right
  :ensure t
  :init
  (back-button-mode 1))

;; (use-package imenu-anywhere
;;   :ensure t
;;   :commands imenu-anywhere
;;   :bind ("C-c C-a" . imenu-anywhere))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;;; select-init.el ends here
