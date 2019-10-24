;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU Emacs configuration
;; (c) Alexander Solovyov 2004-2019
;; alexander AT solovyov.net
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

;;; Use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package bind-key
  :ensure t)

;;; Ocaml forced me to play with paths

(setq opam
  (if (file-executable-p "/usr/local/bin/opam")
      (substring (shell-command-to-string "/usr/local/bin/opam config var prefix 2> /dev/null") 0 -1)
    ""))

(let ((better-path `("~/bin"
                     "/usr/local/go/bin"
                     "/usr/local/sbin"
                     "/usr/local/bin"
                     "/usr/sbin"
                     "/usr/bin"
                     "/sbin"
                     "/bin"
                     "~/dev/go/bin"
                     "~/dev/go/ext/bin"
                     "/usr/local/share/npm/bin"
                     ,(concat opam "/bin"))))
  (when (string-equal "darwin" (symbol-name system-type))
    (setenv "PATH" (mapconcat 'identity better-path ":"))
    (setenv "LANG" "en_US.UTF-8")
    (setenv "CAML_LD_LIBRARY_PATH" "/Users/piranha/.opam/system/lib/stublibs:/usr/local/lib/ocaml/stublibs")
    (setenv "OCAML_TOPLEVEL_PATH" "/Users/piranha/.opam/system/lib/toplevel"))
  (setq exec-path better-path))


(setq custom-file "~/.emacs.d/load/custom-init.el")

(add-to-list 'load-path "~/.emacs.d/load/")
(add-to-list 'load-path "~/.emacs.d/packages/")
(when (not (string= opam ""))
  (add-to-list 'load-path (concat opam "/share/emacs/site-lisp")))

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

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
 '(general frame funs modes ivy keys bs notmuch circe custom))

;;; init.el ends here
