(autoload 'circe "circe" "Sane IRC client" t)

(when (file-directory-p "~/var/circe")
  (add-to-list 'load-path "~/var/circe"))

(setq
 circe-default-nick "piranha"
 circe-default-user "piranha"
 circe-default-realname "Alexander Solovyov"
 circe-ignore-list nil
 circe-server-killed-confirmation 'ask-and-kill-all
 circe-server-auto-join-channels
 '(("^freenode$" "#concatenative" "#conkeror" "#emacs" "#org-mode"))
 circe-nickserv-passwords
 `(("freenode" ,freenode-password)))

(eval-after-load "circe"
  '(progn
     (require 'lui-irc-colors)
     (add-to-list 'lui-pre-output-hook 'lui-irc-colors)
     (require 'lui-logging)
     (setq lui-logging-directory "~/.emacs.d/circe")
     (enable-lui-logging)
     (require 'circe-nickcolor)
     (setq circe-nickcolor-list
           '("NavyBlue" "DarkGreen" "SeaGreen" "ForestGreen" "OliveDrab"
             "SaddleBrown" "brown" "red" "DarkViolet" "yellow4" "DarkRed"
             "brown" "purple"))
     (enable-circe-nickcolor)
     (define-key lui-mode-map (kbd "C-,") (fun-for-bind bs--show-with-configuration "circe"))
     (add-hook 'lui-mode-hook 'no-scroll-margin)))

(defun irc ()
  "Connect to IRC."
  (interactive)
  (circe "irc.freenode.net" "6667" "freenode"))
