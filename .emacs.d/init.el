;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU Emacs configuration
;; (c) Alexander Solovyov 2004-2013
;; alexander AT solovyov.net
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

(let ((better-path "/Users/piranha/bin:/usr/local/go/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/Users/piranha/dev/go/bin:/Users/piranha/dev/go/ext/bin:/usr/local/share/npm/bin:/Users/piranha/.opam/system/bin:/Users/piranha/Library/Haskell/bin"))
  (when (string-equal "darwin" (symbol-name system-type))
    (setenv "PATH" better-path)
    (setenv "CAML_LD_LIBRARY_PATH" "/Users/piranha/.opam/system/lib/stublibs:/usr/local/lib/ocaml/stublibs")
    (setenv "OCAML_TOPLEVEL_PATH" "/Users/piranha/.opam/system/lib/toplevel"))
  (setq exec-path (split-string better-path ":")))

;; initialize el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get/")

(unless (require 'el-get nil t)
    (throw 'not-configured "Install el-get to get dependencies: https://github.com/dimitri/el-get/"))
(setq el-get-sources '())
(defmacro el-get-add (item)
  `(add-to-list 'el-get-sources ',item))

(setq custom-file "~/.emacs.d/load/custom_init.el")

(when (executable-find "opam")
  (add-to-list 'load-path
               (concat
                (replace-regexp-in-string
                 "\n$" ""
                 (shell-command-to-string "opam config var share"))
                "/emacs/site-lisp/")))
(add-to-list 'load-path "~/.emacs.d/load/")
(add-to-list 'load-path "~/.emacs.d/packages/")

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
   bs eshell custom))

;; run el-get now
(el-get 'sync
        (mapcar (lambda (item) (symbol-name (plist-get item :name))) el-get-sources))

;;; init.el ends here
