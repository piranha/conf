;; -*- lexical-binding: t; -*-

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; (use-package buffer-name-relative
;;   :ensure t
;;   :init
;;   (buffer-name-relative-mode)
;;   :config
;;   (setq buffer-name-relative-prefix ""
;;         buffer-name-relative-root-functions
;;         (list 'buffer-name-relative-root-path-from-projectile)))

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

(use-package vertico-prescient
  :ensure t
  :commands (vertico-prescient-mode prescient-persist-mode)
  :init
  (vertico-prescient-mode 1)
  (prescient-persist-mode 1)
  :config
  (setq prescient-filter-method '(literal regexp literal-prefix prefix initialism)
        prescient-sort-full-matches-first t
        prescient-sort-length-enable t
        prescient-history-length 1000))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))


(use-package corfu-prescient
  :ensure t
  :commands corfu-prescient-mode
  :init
  (corfu-prescient-mode 1))

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
  :bind (("M-t" . projectile-find-file)
         :map projectile-mode-map
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
         ;;("C-s" . consult-line)
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
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  (setq consult-narrow-key "<")

  (setq consult-buffer-sources
        '(consult--source-hidden-buffer
          consult--source-modified-buffer
          consult--source-buffer
          consult--source-project-buffer
          consult--source-project-recent-file
          consult--source-recent-file
          consult--source-file-register
          consult--source-bookmark))

  ;; only manual preview for buffer-switching
  (consult-customize consult-buffer :preview-key "M-.")
  (unless 't ;; just a comment
    (consult-customize consult-line :preview-key (list (kbd "M-.")
                                                       :debounce 0 (kbd "<up>") (kbd "<down>")))))

;;; search

(use-package isl
  :ensure t
  :vc (isearch-light :url "https://github.com/thierryvolpiatto/isearch-light")
  :bind (("C-s" . isl-search)
         ("C-M-S" . isl-resume))
  :custom-face
  (isl-match ((t (:inherit query-replace))))
  (isl-match-items ((t (:inherit isearch))))  ; Another search-related face
  (isl-on ((t (:inherit region))))                  ; Current selection

  (isl-number ((t (:inherit font-lock-constant-face)))) ; Often colored appropriately
  (isl-string ((t (:inherit minibuffer-prompt))))   ; Usually bold and colored
  (isl-case-fold ((t (:inherit isl-string)))))

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

;;; select-init.el ends here
