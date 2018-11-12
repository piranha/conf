(defun counsel-ibuffer-kill-buffer (x)
  (kill-buffer (cdr x))
  (ivy-resume))

(use-package counsel
  :ensure t
  :bind (("C-." . counsel-ibuffer)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-c r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("M-A" . counsel-imenu)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-c j" . counsel-git-grep)
         ("C-c s" . counsel-rg)
         ("C-c C-y" . counsel-yank-pop)
         ;("C-c o" . counsel-fzf)
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
  :bind ("C-s" . swiper))

(use-package imenu-anywhere
  :ensure t
  :commands ivy-imenu-anywhere
  :bind ("C-c C-a" . ivy-imenu-anywhere))

(use-package ivy-prescient
  :ensure t
  :init
  (ivy-prescient-mode)
  (prescient-persist-mode)
  (setq ivy-initial-inputs-alist '()))
