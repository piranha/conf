;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU Emacs configuration
;; (c) Alexander Solovyov 2004-2025
;; alexander AT solovyov.net
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))
(package-initialize)

;;; Package management setup

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package bind-key
  :ensure t)

;;; End of package management setup

(setq custom-file "~/.emacs.d/load/custom-init.el")

;;(add-to-list 'load-path "~/.emacs.d/load/") ;; configuration
(add-to-list 'load-path "~/.emacs.d/packages/") ;; custom packages

(let ((default-directory "/opt/homebrew/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(dolist (fname '(general
                 frame
                 funs
                 modes
                 select
                 keys
                 ;;bs
                 ;;notmuch
                 ;;circe
                 custom))
  (load-file (format "~/.emacs.d/load/%s-init.el" fname)))

;;; init.el ends here
