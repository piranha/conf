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
  (mapc #'(lambda (name)
            (load (format "init_%s" name)))
        modules))

(setq custom-file "~/.emacs.d/init/init_custom.el")

(add-to-path 'init)                     ; initialization
(add-to-path 'load)                     ; additional packages
(add-to-path 'load/grep+)
(add-to-path 'load/themes)
;(add-to-list 'load-path (expand-file-name "~/.el"))

(when (file-exists-p "~/.secrets.el")
  (load-file "~/.secrets.el"))

(load-init
 '(general frame lang modes keys funs
   bs eshell jabber circe org custom))
