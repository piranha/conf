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
 '(("freenode" "#mercurial" "#io-ru" "#firepython"))
 circe-nickserv-passwords
 `(("freenode" ,freenode-password))
 circe-nick-next-function 'prh/circe-nick-next)

(defun prh/circe-nick-next (oldnick)
  "Return a new nick to try for OLDNICK."
  (cond
   ;; If the nick ends with digit, increase it
   ((string-match "^\\(.*\\)\\([0-9]\+\\)$" oldnick)
    (concat (match-string 1 oldnick)
            (number-to-string (1+ (string-to-number
                                   (match-string 2 oldnick))))))
   ;; Else just append 1
   (t
    (concat oldnick "1"))))

(eval-after-load "circe"
  '(progn
     (require 'lui-irc-colors)
     (add-to-list 'lui-pre-output-hook 'lui-irc-colors)
     (require 'lui-logging)
     (setq
      lui-logging-directory "~/.emacs.d/circe"
      lui-logging-file-format "{buffer}/%Y-%m-%d.txt")
     (enable-lui-logging)
     (require 'circe-nickcolor)
     (setq circe-nickcolor-list
           '("NavyBlue" "DarkGreen" "SeaGreen" "ForestGreen" "OliveDrab"
             "brown" "red" "purple" "orange red"))
     (enable-circe-nickcolor)
     (define-key lui-mode-map (kbd "C-,") (fun-for-bind bs--show-with-configuration "circe"))
     (add-hook 'lui-mode-hook 'no-scroll-margin)))

(defun irc ()
  "Connect to IRC."
  (interactive)
  (circe "irc.freenode.net" "6667" "freenode"))
