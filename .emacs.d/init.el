;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU Emacs configuration
;; (c) Alexander Solovyov 2004-2012
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

;; initialize el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get/")

(unless (require 'el-get nil t)
    (throw 'not-configured "Install el-get to get dependencies: https://github.com/dimitri/el-get/"))
(setq el-get-sources '())
(defmacro el-get-add (item)
  `(add-to-list 'el-get-sources ',item))

(setq custom-file "~/.emacs.d/load/custom_init.el")

(add-to-path 'load)                     ; initialization
(add-to-path 'packages)                 ; additional packages
(let ((default-directory "~/.emacs.d/packages/"))
  (normal-top-level-add-subdirs-to-load-path))

(load "~/.secrets.el" t)

;;; ELPA
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defun autocompile ()
  "Compile itself if this is config file"
  (interactive)
  (if (or
       (string-match ".emacs.d/load/[a-z]+_init.el$" (buffer-file-name))
       (string-match ".emacs.d/init.el$" (buffer-file-name)))
      (byte-compile-file (buffer-file-name))))

(add-hook 'after-save-hook 'autocompile)

(load-init
 '(general frame funs modes keys
   bs eshell org custom))

;; run el-get now
(el-get)
