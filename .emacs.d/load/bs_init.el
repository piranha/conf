(add-hook 'bs-mode-hook 'no-scroll-margin)
(autoload 'bs--show-with-configuration "bs")

(defmacro bs-filter (modes)
  `(lambda (buf)
     (with-current-buffer buf
       (not (memq major-mode ,modes)))))

(defun bs-conf (name keywords)
  "Add a configuration to bs-configurations.
Retrieves two arguments: NAME and KEYWORDS plist in next format:

'(:show-re nil :show-f nil :hide-re nil :hide-f nil :sort nil)

All of key-value pairs in KEYWORDS are optional.

Read documentation of `bs-configurations' for more details."
  (list name
        (plist-maybe-get keywords :show-re)
        (plist-maybe-get keywords :show-f)
        (plist-maybe-get keywords :hide-re)
        (plist-maybe-get keywords :hide-f)
        (plist-maybe-get keywords :sort)))

(setq bs-configurations
      `(,(bs-conf "files"  `(:hide-f files-without-org
                             :sort bs-sort-buffer-interns-are-last))
        ,(bs-conf "org"    `(:hide-f ,(bs-filter '(org-mode))))
        ,(bs-conf "jabber" `(:hide-f ,(bs-filter '(jabber-chat-mode
                                                   jabber-roster-mode))))
        ,(bs-conf "circe"  `(:hide-f ,(bs-filter '(circe-channel-mode
                                                   circe-server-mode
                                                   circe-query-mode))))
        ,(bs-conf "dired"  `(:hide-f ,(bs-filter '(dired-mode))))
        ,(bs-conf "all" '())))

(setq bs-default-configuration "files")
(setq bs-alternative-configuration "all")

(defun files-without-org (buffer)
  "Return true when buffer is file and not org-mode"
  (or
   (not (buffer-file-name buffer))
   (with-current-buffer buffer (eq major-mode 'org-mode))))

(defun bs-switch-to-files-and-refresh ()
  "Apply \"files\" configuration. Refresh whole Buffer Selection Menu."
  (interactive)
  (bs-set-configuration "files")
  (setq bs-default-configuration bs-current-configuration)
  (bs--redisplay t)
  (bs--set-window-height))

(eval-after-load "bs"
  '(define-key bs-mode-map "z" 'bs-switch-to-files-and-refresh))
