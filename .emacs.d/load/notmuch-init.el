(autoload 'notmuch "notmuch" "notmuch mail" t)

(global-set-key (kbd "C-c e") 'notmuch)

(setq notmuch-archive-tags '("-inbox" "+archive")
      notmuch-multipart/alternative-discouraged '("text/plain" "text/html")
      notmuch-show-logo nil
      mm-text-html-renderer 'w3m-standalone

      message-kill-buffer-on-exit t
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp"
      message-sendmail-f-is-evil t)

(setq notmuch-saved-searches
      '((:name "flagged" :key "f" :query "tag:flagged and not tag:archive")
        (:name "inbox"   :key "i" :query "tag:inbox")
        (:name "info"    :key "n" :query "tag:info and not tag:archive")
        (:name "unread"  :key "u" :query "'tag:unread and not (tag:errors or tag:archive)'")
        (:name "support" :key "s" :query "tag:support and not tag:archive")
        (:name "git"     :key "g" :query "tag:git and tag:unread and not tag:archive")
        (:name "errors"  :key "e" :query "tag:errors and not tag:archive")
        (:name "drafts"  :key "d" :query "tag:draft")
        (:name "since yesterday" :query "date:yesterday..")))

(with-eval-after-load 'notmuch
  (define-key notmuch-show-mode-map "l" 'notmuch-show-add-tag)
  (define-key notmuch-search-mode-map "l" 'notmuch-search-add-tag)

  (define-key notmuch-show-mode-map "y" 'notmuch-show-archive-thread-then-exit)
  (define-key notmuch-search-mode-map "y" 'notmuch-search-archive-thread)
  (define-key notmuch-tree-mode-map "y" 'notmuch-tree-archive-thread)

  (define-key notmuch-hello-mode-map "q"
    '(lambda () (interactive) (bury-buffer)))
  (define-key notmuch-show-mode-map "q"
    '(lambda () (interactive)
       (notmuch-bury-or-kill-this-buffer)
       (if (and (eq major-mode 'notmuch-search-mode)
                (< (buffer-size) 1024))
           (notmuch-search-refresh-view))))

  (define-key notmuch-search-mode-map "c" 'notmuch-mua-new-mail)
  (define-key notmuch-show-mode-map "c" 'notmuch-mua-new-mail)
  (define-key notmuch-hello-mode-map "c" 'notmuch-mua-new-mail)

  (define-key notmuch-search-mode-map "g" 'notmuch-jump-search)
  (define-key notmuch-show-mode-map "g" 'notmuch-jump-search)
  (define-key notmuch-hello-mode-map "g" 'notmuch-jump-search)

  (define-key notmuch-show-mode-map "j" 'notmuch-show-next-thread-show)
  (define-key notmuch-search-mode-map "j" 'notmuch-search-next-thread)
  (define-key notmuch-show-mode-map "k" 'notmuch-show-previous-thread-show)
  (define-key notmuch-search-mode-map "k" 'notmuch-search-previous-thread)

  (define-key notmuch-show-mode-map "D" (lambda () (interactive) (notmuch-show-tag '("+deleted"))))
  (define-key notmuch-search-mode-map "D" (lambda () (interactive) (notmuch-search-tag '("+deleted"))))

  (set-face-attribute 'notmuch-message-summary-face nil
                      :bold t
                      :background "#cccccc"
                      :underline "black"))

(defvar notmuch-hello-refresh-count 0)
(defvar notmuch-hello-message "")

(defun notmuch-hello-new-count ()
  (let* ((new-count
          (string-to-number (car (process-lines notmuch-command "count"))))
         (diff-count (- new-count notmuch-hello-refresh-count)))

    (setq notmuch-hello-message
          (cond
           ((= notmuch-hello-refresh-count 0)
            (format "You have %s messages."
                    (notmuch-hello-nice-number new-count)))
           ((> diff-count 0)
            (format "You have %s more messages since last refresh."
                    (notmuch-hello-nice-number diff-count)))
           ((< diff-count 0)
            (format "You have %s fewer messages since last refresh."
                    (notmuch-hello-nice-number (- diff-count))))
           (t "")))

    (setq notmuch-hello-refresh-count new-count)
    (if (= new-count (- diff-count))
        new-count
      diff-count)))

(defun notmuch-hello-refresh-status-message ()
  (when (not (eq "" notmuch-hello-message))
    (message notmuch-hello-message)
    (setq notmuch-hello-message "")))

(add-hook 'notmuch-hello-refresh-hook 'notmuch-hello-refresh-status-message)

(defun notmuch-refresh-maybe ()
  (when (get-buffer "*notmuch-hello*")
    (cl-letf (((symbol-function 'notmuch-hello-refresh-status-message) #'ignore))
      (notmuch-hello-update t)
      (when (not (zerop (notmuch-hello-new-count)))
        (tracking-add-buffer (get-buffer "*notmuch-hello*"))))))

;; BBDB

(el-get-bundle bbdb
  :type http-tar
  :options ("xf")
  :build (("env"
           "EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs"
           "EMACSLOADPATH=/Applications/Emacs.app/Contents/Resources/lisp"
           "./configure")
          ("sed" "-i" "" "s/DATA = bbdb.pdf/DATA = #bbdb.pdf/" "doc/Makefile")
          ("make"))
  :load-path ("./lisp")
  :url "http://download.savannah.gnu.org/releases/bbdb/bbdb-3.1.2.tar.gz")

(setq bbdb-file "~/.emacs.d/bbdb"
      bbdb-user-mail-address-re "o.solovyov@modnakasta.ua|os@modnakasta.ua"
      bbdb-update-records-p t
      bbdb-add-aka t
      bbdb-mua-auto-update-p 'bbdb-select-message
      bbdb-mua-pop-up nil
      bbdb-completion-display-record nil
      bbdb-ignore-message-alist '(("From" . "gitlab@git.modnakasta.ua")
                                  ("From" . "jira@modnakasta.ua")
                                  ("From" . "confluence@modnakasta.ua")
                                  ("From" . "@mailer.aha.io")))

;; Override BBDB internal functions to catch all addresses from notmuch mails

(defun prh/bbdb-message-header (header)
  (notmuch-show-get-header (intern-soft (concat ":" (capitalize header)))))

(defun prh/bbdb-mua ()
  'notmuch)

(defun prh/bbdb-mua-update-records (&optional header-class update-p sort)
  (save-current-buffer
    (bbdb-update-records (bbdb-get-address-components header-class)
                         update-p sort)))

(defun prh/bbdb-mua-auto-update (&optional header-class update-p)
  (interactive)
  (cl-letf (((symbol-function 'bbdb-mua-update-records) #'prh/bbdb-mua-update-records)
            ((symbol-function 'bbdb-mua) #'prh/bbdb-mua)
            ((symbol-function 'bbdb-message-header) #'prh/bbdb-message-header))
    (bbdb-mua-auto-update header-class update-p)))

(with-eval-after-load 'notmuch
  (bbdb-initialize 'message)
  (bbdb-mua-auto-update-init 'message)
  (add-hook 'notmuch-show-hook 'prh/bbdb-mua-auto-update)
  (remove-hook 'message-send-hook 'bbdb-mua-auto-update)
  (add-hook 'message-send-hook 'prh/bbdb-mua-auto-update))
