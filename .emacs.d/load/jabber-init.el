(autoload 'jabber-connect-all "jabber" "Emacs Jabber client" t)
(global-set-key (kbd "C-x C-j C-c") 'jabber-connect-all)

(setq
 jabber-account-list `(("piranha@eth0.net.ua/emagz"
                        (:password . ,eth0-jabber)))
 jabber-roster-line-format " %-25n %u %-8s  %S"
 jabber-chat-buffer-show-avatar nil
 jabber-history-enabled t
 jabber-use-global-history nil
 jabber-history-dir "~/.emacs.d/jabber/"
 jabber-chat-buffer-format "*%n*"
 jabber-groupchat-buffer-format "*%n-muc*")

(with-eval-after-load 'jabber
  (add-hook 'jabber-chat-mode-hook 'no-scroll-margin)

  (define-key jabber-chat-mode-map (kbd "C-,")
    (fun-for-bind bs--show-with-configuration "jabber"))
  (define-key jabber-roster-mode-map (kbd "C-,")
    (fun-for-bind bs--show-with-configuration "jabber"))

  (add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)
  (setq jabber-autoaway-method 'jabber-xprintidle-program)

  (global-set-key (kbd "<f12>") (fun-for-bind toggle-buffer "*-jabber-*"))

  (remove-hook 'jabber-alert-message-hooks 'jabber-message-echo)
  (remove-hook 'jabber-alert-muc-hooks 'jabber-muc-echo)
  (remove-hook 'jabber-alert-presence-hooks 'jabber-presence-echo)
  (remove-hook 'jabber-alert-info-message-hooks 'jabber-info-echo))
