(when (file-directory-p "~/var/jabber.el")
  (add-to-list 'load-path "~/var/jabber.el"))
(autoload 'jabber-connect "jabber" "Emacs Jabber client" t)
(global-set-key (kbd "C-x C-j C-c") 'jabber-connect)

(setq
 jabber-account-list `(("asolovyov@mydeco.com/emagz"
                        (:network-server . "chat.mydeco.com")
                        (:port . 5223)
                        (:connection-type . ssl)
                        (:password . ,mydeco-jabber))
                       ("piranha@eth0.net.ua/emagz"
                        (:password . ,eth0-jabber)))
 jabber-muc-autojoin '("dreamteam@conference.mydeco.com" "ui@conference.mydeco.com")
 jabber-roster-line-format " %-25n %u %-8s  %S"
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

;;      (defcustom jabber-scroll-to-bottom-p t
;;        "Non-nil if jabber should keep the input line at the end of the window."
;;        :type 'boolean
;;        :group 'jabber-chat)

;;      (defun jabber-scroll-to-bottom (window display-start)
;;        "Scroll the input line to the bottom of the window."
;;        (when (and window
;;                   jabber-scroll-to-bottom-p)
;;          (let ((resize-mini-windows nil))
;;            ;; This is to prevent an XEmacs byte compilation warning
;;            ;; "variable bound but not referred to". XEmacs is trying to be
;;            ;; too intelligent.
;;            (when (featurep 'xemacs)
;;              (declare (special resize-mini-windows)))
;;            (save-selected-window
;;              (select-window window)
;;              (save-restriction
;;                (widen)
;;                (when (>= (point) jabber-point-insert)
;;                  (save-excursion
;;                    (goto-char (point-max))
;;                    (recenter -1)
;;                    (sit-for 0))))))))

;;      (add-hook 'jabber-chat-mode-hook
;;                (lambda ()
;;                  (add-hook 'window-scroll-functions
;;                            'jabber-scroll-to-bottom
;;                            nil t)))

     (global-set-key (kbd "<f12>") (fun-for-bind toggle-buffer "*-jabber-*"))

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
         (let ((title (car (split-string from "@"))))
           (jabber-notify-display title text)))

       (defun jabber-muc-notify (nick group buffer text proposed-alert)
         (let ((group-name (car (split-string group "@"))))
           (let ((title (concat nick "@" group-name)))
             (jabber-notify-display title text))))

       (define-personal-jabber-alert jabber-muc-notify)

       (setq jabber-alert-message-hooks '(jabber-message-scroll
                                          jabber-message-notify))
       (setq jabber-alert-muc-hooks '(jabber-muc-scroll
                                      jabber-muc-notify-personal))
       (setq jabber-alert-presence-hooks '())
       (setq jabber-alert-info-message-hooks '(jabber-info-display))
       )
     ))
