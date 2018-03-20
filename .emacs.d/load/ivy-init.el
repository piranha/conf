(use-package counsel
  :ensure t
  :bind (("C-." . ivy-switch-buffer)
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
         :map read-expression-map
         ("C-r" . counsel-minibuffer-history))
  :init
  (setq ivy-use-virtual-buffers t
        ivy-display-style 'fancy)
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package imenu-anywhere
  :ensure t
  :commands ivy-imenu-anywhere
  :bind ("C-c C-a" . ivy-imenu-anywhere))
