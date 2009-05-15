;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU Emacs configuration
;; (c) Alexander Solovyov 2004-2008
;; piranha AT piranha.org.ua
;;
;; Thanks to all, who has helped me in creation, especially to:
;; Yuriy Sazonets
;; Alex Ott
;; Emacswiki.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-to-path (dir)
  (add-to-list 'load-path
               (format "~/.emacs.d/%s" dir)))

(defun load-init (modules)
  "Load initialization files"
  (mapc (lambda (name)
          (load (format "%s_init" name)))
        modules))

(defmacro fun-for-bind (func &rest args)
  "Returns a symbol of an anonymous interactive function,
suitable for binding to keys."
  `(lambda () (interactive) (,func ,@args)))

(setq custom-file "~/.emacs.d/load/custom_init.el")

(add-to-path 'load)                     ; initialization
(add-to-path 'packages)                 ; additional packages
(let ((default-directory "~/.emacs.d/packages/"))
  (normal-top-level-add-subdirs-to-load-path))

(when (file-exists-p "~/.secrets.el")
  (load-file "~/.secrets.el"))

;;; ELPA
(when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(defun autocompile ()
  "Compile itself if this is config file"
  (interactive)
  (if (or
       (string-match ".emacs.d/load/[a-z]+_init.el$" (buffer-file-name))
       (string-match ".emacs.d/init.el$" (buffer-file-name)))
      (byte-compile-file (buffer-file-name))))

(add-hook 'after-save-hook 'autocompile)

(load-init
 '(general frame lang funs modes keys
   bs eshell jabber circe org custom))
