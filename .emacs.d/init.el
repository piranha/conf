;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU Emacs configuration
;; (c) Alexander Solovyov 2004-2020
;; alexander AT solovyov.net
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

;;; Package management setup

(eval-when-compile
  (unless (fboundp 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

(use-package bind-key
  :ensure t)

;;; End of package management setup


(let ((better-path `("~/bin"
                     "/opt/homebrew/sbin"
                     "/opt/homebrew/bin"
                     "/usr/local/sbin"
                     "/usr/local/bin"
                     "/usr/sbin"
                     "/usr/bin"
                     "/sbin"
                     "/bin"
                     "~/dev/go/bin"
                     "~/dev/go/ext/bin"
                     "/usr/local/share/npm/bin")))
  (when (string-equal "darwin" (symbol-name system-type))
    (setenv "PATH" (mapconcat 'identity better-path ":"))
    (setenv "LANG" "en_US.UTF-8"))
  (setq exec-path better-path))


(setq custom-file "~/.emacs.d/load/custom-init.el")
(let ((secrets-el "~/.secrets.el"))
  (when (file-exists-p secrets-el)
    (load-file secrets-el)))


(add-to-list 'load-path "~/.emacs.d/load/") ;; configuration
(add-to-list 'load-path "~/.emacs.d/packages/") ;; custom packages

(let ((default-directory "/opt/homebrew/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(defun autocompile ()
  "Compile itself if this is config file."
  (interactive)
  (if (or
       (string-match ".emacs.d/load/[a-z]+-init.el$" (buffer-file-name))
       (string-match ".emacs.d/init.el$" (buffer-file-name)))
      (byte-compile-file (buffer-file-name))))

(add-hook 'after-save-hook 'autocompile)

(defun load-init (modules)
  "Load initialization files.

  MODULES - configuration packages to load"
  (mapc (lambda (name)
          (load (format "%s-init" name)))
        modules))

(load-init
 '(general
   frame
   funs
   modes
   select
   keys
   bs
   ;;notmuch
   ;;circe
   custom))

;;; init.el ends here
