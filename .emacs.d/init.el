;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU Emacs configuration
;; (c) Alexander Solovyov 2004-2013
;; alexander AT solovyov.net
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (throw 'not-configured
         "Install el-get to get dependencies: https://github.com/dimitri/el-get/"))
(setq el-get-git-shallow-clone t)

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
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

(defun autocompile ()
  "Compile itself if this is config file"
  (interactive)
  (if (or
       (string-match ".emacs.d/load/[a-z]+-init.el$" (buffer-file-name))
       (string-match ".emacs.d/init.el$" (buffer-file-name)))
      (byte-compile-file (buffer-file-name))))

(add-hook 'after-save-hook 'autocompile)

(defun load-init (modules)
  "Load initialization files"
  (mapc (lambda (name)
          (load (format "%s-init" name)))
        modules))

(load-init
 '(general frame funs modes ivy keys bs eshell notmuch circe custom))

;;; init.el ends here
