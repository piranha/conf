(defun counsel-ibuffer-kill-buffer (x)
  (kill-buffer (cdr x))
  (ivy-resume))

(use-package counsel
  :ensure t
  :bind (;;("C-." . counsel-ibuffer)
         ("C-." . ivy-switch-buffer)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-c r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("M-A" . counsel-imenu)
         ("C-x C-f" . counsel-find-file)
         ("C-c C-x C-f" . counsel-recentf)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-c j" . counsel-git-grep)
         ("C-c s" . counsel-rg)
         ("C-c C-y" . counsel-yank-pop)
         ("C-c C-m" . counsel-mark-ring)
         ;;("C-c o" . counsel-fzf)
         :map read-expression-map
         ("C-r" . counsel-minibuffer-history))
  :init
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        ivy-display-style 'fancy)
  (setenv "FZF_DEFAULT_COMMAND" "fd -t f")
  ;; (ivy-add-actions
  ;;  'counsel-ibuffer
  ;;  '(("k" counsel-ibuffer-kill-buffer "kill buffer")))
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper-isearch))

(use-package imenu-anywhere
  :ensure t
  :commands ivy-imenu-anywhere
  :bind ("C-c C-a" . ivy-imenu-anywhere))

(use-package ivy-prescient
  :ensure t
  :commands ivy-prescient-mode
  :init
  (ivy-prescient-mode)
  (prescient-persist-mode)
  (setq ivy-initial-inputs-alist '()))

(use-package counsel-projectile
  :ensure t
  :commands counsel-projectile-mode
  :init
  (counsel-projectile-mode))

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
