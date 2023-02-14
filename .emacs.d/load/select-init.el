(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; (use-package relative-buffers
;;   :ensure t
;;   :config
;;   (global-relative-buffers-mode))

;; (use-package ctrlf
;;   :ensure t
;;   :commands ctrlf-mode
;;   :init
;;   (ctrlf-mode 1))

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

(use-package deadgrep
  :ensure t
  :commands (deadgrep--read-search-term)
  :bind (("C-c s" . deadgrep)))


(defun projectile-deadgrep (search-term)
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
         ("s s" . projectile-deadgrep))
  :init
  (projectile-mode 1)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'default))


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


;;; kill ring

;; (defun konix/kill-ring-insert ()
;;   (interactive)
;;   (let* ((selectrum-should-sort nil)
;;          (toinsert (completing-read "Yank : "
;;                                     (delete-dups kill-ring))))
;;     (when (and toinsert (region-active-p))
;;       ;; the currently highlighted section is to be replaced by the yank
;;       (delete-region (region-beginning) (region-end)))
;;     (insert toinsert)))

;; (global-set-key (kbd "C-c C-y") #'konix/kill-ring-insert)


(use-package consult
  :ensure t
  :bind (("C-." . consult-buffer)
         ("C-s" . consult-line)
         ("C-c C-k" . consult-kmacro)
         ("M-y" . consult-yank-pop)
         ("C-x r b" . consult-bookmark) ;; bookmark-jump
         ("M-g M-g" . consult-goto-line)
         ("M-g m" . consult-mark)
         ("M-g M-m" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g M-i" . consult-imenu-multi)
         ("M-g o" . consult-outline))
  :config
  (setq consult-project-function (lambda (_) (projectile-project-root)))

  ;; only manual preview for buffer-switching
  (consult-customize consult-buffer :preview-key (kbd "M-."))
  (unless 't ;; just a comment
    (consult-customize consult-line :preview-key (list (kbd "M-.")
                                                       :debounce 0 (kbd "<up>") (kbd "<down>")))))


;; (use-package imenu-anywhere
;;   :ensure t
;;   :commands imenu-anywhere
;;   :bind ("C-c C-a" . imenu-anywhere))
