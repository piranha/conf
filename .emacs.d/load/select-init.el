(use-package ctrlf
  :ensure t
  :bind (:map ctrlf--keymap
         ([remap isearch-forward ] . ctrlf-forward-fuzzy)
         ([remap isearch-backward] . ctrlf-backward-fuzzy))
  :config
  (ctrlf-mode 1))

(use-package selectrum
  :ensure t
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :ensure t
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


;;; kill ring

(defun konix/kill-ring-insert ()
  (interactive)
  (let* ((selectrum-should-sort-p nil)
         ;;(selectrum-refine-candidates-function #'selectrum-default-candidate-refine-function)
         (toinsert (completing-read "Yank : "
                                    (delete-dups kill-ring))))
    (when (and toinsert (region-active-p))
      ;; the currently highlighted section is to be replaced by the yank
      (delete-region (region-beginning) (region-end)))
    (insert toinsert)))

(global-set-key (kbd "C-c C-y") #'konix/kill-ring-insert)


(use-package imenu-anywhere
  :ensure t
  :commands imenu-anywhere
  :bind ("C-c M-A" . imenu-anywhere))
