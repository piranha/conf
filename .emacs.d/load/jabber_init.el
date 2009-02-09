(when (file-directory-p "~/var/jabber.el")
  (add-to-list 'load-path "~/var/jabber.el"))
(autoload 'jabber-connect-all "jabber" "Emacs Jabber client" t)
(global-set-key (kbd "C-x C-j C-c") 'jabber-connect-all)

(setq
 jabber-account-list `(("piranha@eth0.net.ua/emagz"
                        (:password . ,eth0-jabber))
                       ("asolovyov@rainboo.com/emagz"
                        (:password . ,rainboo-jabber)))
 jabber-roster-line-format " %-25n %u %-8s  %S"
 jabber-chat-buffer-show-avatar nil
 jabber-history-enabled t
 jabber-use-global-history nil
 jabber-history-dir "~/.emacs.d/jabber/"
 jabber-chat-buffer-format "*%n*"
 jabber-groupchat-buffer-format "*%n-muc*"
 )

(eval-after-load "jabber"
  '(progn
     (add-hook 'jabber-chat-mode-hook 'no-scroll-margin)

     (define-key jabber-chat-mode-map (kbd "C-,") (fun-for-bind bs--show-with-configuration "jabber"))
     (define-key jabber-roster-mode-map (kbd "C-,") (fun-for-bind bs--show-with-configuration "jabber"))

     (add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)
     (setq jabber-autoaway-method 'jabber-xprintidle-program)

     (global-set-key (kbd "<f12>") (fun-for-bind toggle-buffer "*-jabber-*"))

     (remove-hook 'jabber-alert-message-hooks 'jabber-message-echo)
     (remove-hook 'jabber-alert-muc-hooks 'jabber-muc-echo)
     (remove-hook 'jabber-alert-presence-hooks 'jabber-presence-echo)
     (remove-hook 'jabber-alert-info-message-hooks 'jabber-info-echo)

     (when nix
       (setq jabber-notify-display-time 3)
       (setq jabber-notify-max-length 30)

       (defun jabber-notify-display (title text)
         "Displays MESSAGE with TITLE through the libnotify"
         (let (
               (process-connection-type nil)
               (length (* 1000 jabber-notify-display-time))
               (message (if (> (length text) jabber-notify-max-length)
                            (concat (substring text 0 jabber-notify-max-length) "...")
                          text))
               )
           (start-process "jabber-notify" nil "notify-send" "-t" (number-to-string length) title message)
           (process-send-eof "jabber-notify")))

       (defun jabber-message-notify (from buffer text proposed-alert)
         (if (member *jabber-current-show* '("" "chat"))
             (let ((title (car (split-string from "@"))))
               (jabber-notify-display title text))))

       (defun jabber-muc-notify (nick group buffer text proposed-alert)
         (message "%s" group)
         (if (member *jabber-current-show* '("" "chat"))
             (let ((group-name (car (split-string group "@"))))
               (let ((title (concat nick "@" group-name)))
                 (jabber-notify-display title text)))))

       (define-personal-jabber-alert jabber-muc-notify)

       (add-hook 'jabber-alert-message-hooks 'jabber-message-notify)
       (add-hook 'jabber-alert-muc-hooks 'jabber-muc-notify-personal)
       )
     ))
