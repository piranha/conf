(el-get-bundle circe)

(defun prh/nickserv-password (_)
  (with-temp-buffer
    (insert-file-contents-literally "~/.secrets.el")
    (plist-get (read (buffer-string)) :freenode-password)))

(setq circe-network-options
      `(("Freenode"
         :nick "piranha"
         :channels ("#clojure" "#clojurescript" "#hoplon")
         :nickserv-password prh/nickserv-password)))

(setq circe-use-cycle-completion t
      circe-reduce-lurker-spam t
      circe-default-user "piranha"
      circe-default-realname "Alexander Solovyov"
      circe-server-killed-confirmation 'ask-and-kill-all

      lui-time-stamp-position 'right-margin
      lui-time-stamp-format "%H:%M")

(with-eval-after-load "circe"
  (require 'lui-irc-colors)
  (add-to-list 'lui-pre-output-hook 'lui-irc-colors)

  (require 'lui-logging)
  (setq lui-logging-directory "~/.emacs.d/circe"
        lui-logging-file-format "{buffer}/%Y-%m-%d.txt")
  (enable-lui-logging-globally)

  (require 'circe-color-nicks)
  (enable-circe-color-nicks)

  (add-hook 'lui-mode-hook 'my-circe-set-margin)
  (defun my-circe-set-margin ()
    (setq right-margin-width 5))

  (define-key lui-mode-map (kbd "C-,") (fun-for-bind bs--show-with-configuration "circe"))
  (add-hook 'lui-mode-hook 'no-scroll-margin)

  (circe-lagmon-mode))

(defun irc ()
  "Connect to IRC."
  (interactive)
  (circe "Freenode"))
