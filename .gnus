;; -*- mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gnus config
;; (c) Alexander Solovyov 2004-2008
;; piranha AT piranha.org.ua
;;
;; Thanks to all, who has helped me in creation, especially to:
;; Yuriy Sazonets
;; Alexander Zayats
;; Emacswiki.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; graphics
;;;;;;;;;;;

;; defined also in .emacs, but to be sure...
(defconst graf
  (not (eq window-system 'nil))
  "Are we running window system?")

;;;;;;;;;;
;; Loading
;;;;;;;;;;
(when (file-exists-p "~/.secrets.el")
  (load "~/.secrets.el"))

;;;;;;;;;;;
;; Language
;;;;;;;;;;;

(setq mm-body-charset-encoding-alist '((koi8-u . 8bit)))
(setq gnus-group-posting-charset-alist nil)
(setq gnus-group-charset-alist nil)
(push '(".*" koi8-u t) gnus-group-posting-charset-alist)
(push '(".*" koi8-u t) gnus-group-charset-alist)

;;;;;;;;;;
;; General
;;;;;;;;;;

;; auto word-wrap
(add-hook 'message-mode-hook
	(lambda ()
      (setq fill-column 72)
      (turn-on-auto-fill)))

(add-hook 'message-mode-hook 'turn-on-orgstruct++)

;; remove thouse ugly red line endings
(defun disable-trailing-whitespace ()
 (setq show-trailing-whitespace nil))
(add-hook 'gnus-summary-mode-hook 'disable-trailing-whitespace)
(add-hook 'gnus-group-mode-hook 'disable-trailing-whitespace)
(add-hook 'gnus-article-mode-hook 'disable-trailing-whitespace)

(setq
  ;; be verbose
  gnus-novice-user nil
  ;; where move expired messages
  nnmail-expiry-target "nnml:expired"
  ;; don't read whole active file (see help for add info)
  gnus-read-active-file nil
  ;; ask server for new newsgroups
  gnus-check-new-newsgroups nil
  ;; smileys suck
  gnus-treat-display-smileys nil
  ;; all cites must be highlighted
  gnus-cite-minimum-match-count 1
  ;; gnus directory :))
  gnus-directory "~"
  ;; how many messages must has group, for asking when entering
  gnus-large-newsgroup 300
  ;; split long outgoing mail
;  message-send-mail-partially-limit 380000
  ;; highlight only good signatures
  gnus-signature-separator "^-- |______+$"
  ;; don't insert Cancel-Lock
  message-insert-canlock nil
)

;; User settings
(setq
  user-name "piranha"
  user-full-name "Alexander Solovyov"
  user-mail-address "piranha@piranha.org.ua"
  message-alternative-emails
  (regexp-opt '("asolovyov@rainboo.com" "alexander.solovyov@gmail.com" "alexander.solovyov@willowcode.com"))
)

;; Send mail via sendmail
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;; Send mail via SMTP
;(setq
;  gnus-local-domain "piranha.org.ua"
;  smtpmail-default-smtp-server "viii.ntu-kpi.kiev.ua"
;  smtpmail-local-domain "viii.ntu-kpi.kiev.ua"
;  smtpmail-default-smtp-server "localhost"
;  message-send-mail-function 'smtpmail-send-it
;  smtpmail-auth-login-username "piranha"
;)

;(setq smtpmail-debug-info t)

(autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
(add-hook 'message-setup-hook 'gnus-alias-determine-identity)
(add-hook 'message-load-hook '(lambda () (gnus-alias-init)))

(setq gnus-alias-identity-alist
      '(
        ;;(name    inherit    from    organization    headers    body    signature)
        ("piranha"  nil "\"Alexander Solovyov\" <piranha@piranha.org.ua>" nil nil nil "~/.signature")
        ("news"     "piranha" nil nil (("X-keywords" . x-keyword)) nil nil)
        ("viii"     "piranha" "\"Alexander Solovyov\" <piranha@viii.ntu-kpi.kiev.ua>" nil nil nil nil)
        ("gmail"    "piranha" "\"Alexander Solovyov\" <alexander.solovyov@gmail.com>" nil nil nil nil)
        ))
(setq gnus-alias-identity-rules
     '(
       ("ntu-kpi" ("newsgroup" "^ntu-kpi" both) "news")
       ))
(setq gnus-alias-default-identity "piranha")

;; gnus-parameters
(setq gnus-parameters
      '((".*"
         (gnus-show-threads t)
         (gnus-use-scoring nil)
;         (display . all)
)))

;; Show text before html
(eval-after-load "mm-decode"
 '(progn
      (add-to-list 'mm-discouraged-alternatives "text/html")
      (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;;;;;;;
;; News
;;;;;;;

(setq gnus-select-method  '(nntp "news.ntu-kpi.kiev.ua"))
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;(setq gnus-select-method '(nnnil ""))

;; headers displayed
(setq gnus-visible-headers
 '("From:"
   "^Newsgroups:"
   "^Reply-To:"
   "^Subject:"
;   "^Message-ID:"
   "^Date:"
   "^To:"
   "^Cc:"
   "^Posted-To:"
   "^Mail-Copies-To:"
   "^Resent-From:"
   "^X-Sent:"
;   "^User-Agent:"
;   "^Content-Type:"
   "^X-Newsreader:"
   "^NNTP-Posting-Host:"
   "^Followup-To:"
))

;; message forwarding
(setq
   message-forward-as-mime nil
   message-forward-ignored-headers "^Content-Transfer-Encoding:\\|^X-\\|^References\\|^Xref\\|^In-Reply-To\\|^Received\\|^NNTP\\|^Return-Path\\|^Sender\\|^Approved\\|^User-Agent\\|^List-\\|^Error-To\\|^FL-\\|^Precedence\\|^Gnus-\\|^Path\\|^Cancel-Lock\\|^Face")


;;;;;;;
;; Mail
;;;;;;;

(setq mail-sources nil)
(setq gnus-secondary-select-methods
      '((nnimap "mail"
                (nnimap-address "localhost")
                (nnimap-stream network))))

;(setq imap-ssl-program "openssl s_client -tls1 -connect %s:%p")
;; (setq gnus-secondary-select-methods
;;      '((nnimap "imap.gmail.com"
;;                (nnimap-address "imap.gmail.com")
;;                (nnimap-authinfo-file "~/.imap-authinfo")
;;                (nnimap-server-port 993)
;;                (nnimap-stream ssl)
;; ;               (nnimap-list-pattern ("INBOX" ".*"))
;;                )))

(defun prh:mail-date (u) (concat u (format-time-string ".%Y.%m" (current-time))))
(defun prh:mail-date2 (u) (concat u (format-time-string ".%Y" (current-time))))

;(setq nnmail-split-methods 'nnmail-split-fancy)
;(setq nnmail-split-fancy
;      '(|
;        ("\\(To\\|Cc\\)" ".*hostels.*@ntu-kpi\\.kiev\\.ua.*" "hostels")
;        ("\\(To\\|Cc\\)" ".*all-admins@ntu-kpi\\.kiev\\.ua.*" "hostels")
;        ("\\(To\\|Cc\\)" ".*humor@xcp\\.kiev\\.ua.*" (: prh:mail-date2 "humor"))
;        ("From" ".*daily@security\\.nnov\\.ru.*" "security")
;        ("\\(To\\|Cc\\)" ".*anime_kpi@yahoogroups.com.*" "anime_kpi")
;        ("From" ".*Alexander Solovyov \\[Moderator\\].*" "moder")
;		 ("From" ".*Nikita Gubenko \\[CoModerator\\].*" "moder")
;        ("List-Id" ".*c-p-c.googlegroups.com.*" "cpc")
;        ("List-Id" ".*newstalk.news.ntu-kpi.kiev.ua.*" "news-talk")
;		 ("List-Id" ".*exim-users.exim.org.ua.*" "exim")
;		 ("List-Id" ".*eth0.googlegroups.com.*" "eth0")
;		 ("List-Id" ".*users.lists.eth0.net.ua.*" "eth0-public")
;		 ("List-Id" ".*sudoers.lists.eth0.net.ua.*" "eth0-sudoers")
;		 ("List-Id" ".*ik22.lists.eth0.net.ua.*" "ik22")
;		 ("From" ".*@livejournal.com.*" "lj")
;		 ("From" ".*root@eth0.net.ua.*" "root")
;		 ("Subject" ".*\\*\\*\\*SPAM\\*\\*\\*.*" "spam")
;        (any ".*" (: prh:mail-date "inbox"))
;        ))


;; save messages
;(setq gnus-message-archive-group '(
;    (if (message-news-p) (concat "nnimap+piranha.org.ua:sent-news" (format-time-string ".%Y.%m" (current-time)))
;      (concat "nnimap+piranha.org.ua:sent" (format-time-string ".%Y.%m" (current-time))))))
(defun prh:archive-mailbox-date (mailbox)
  (concat mailbox (format-time-string ".%Y.%m" (current-time))))
(setq gnus-message-archive-group '(
     (if (message-news-p) (prh:archive-mailbox-date "nnimap+mail:sent-news")
       (prh:archive-mailbox-date "nnimap+mail:sent-mail"))))

;;;;;;;;;;;;
;; Threading
;;;;;;;;;;;;

;   If `some', only fill in the gaps that are needed to tie loose threads
;  together.  If `more', fill in all leaf nodes that Gnus can find.  If
;  non-nil and non-`some', fill in all gaps that Gnus manages to guess.
(setq gnus-build-sparse-threads 'more)

;; build threads by references, not subj
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

;; summary buffer formatting
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-date))
;	gnus-thread-sort-by-number
;	gnus-thread-sort-by-subject
;	gnus-thread-sort-by-author
;	gnus-thread-sort-by-score
;	gnus-thread-sort-by-total-score))

;; yet another threading
(setq gnus-summary-line-format
      ":%U%R| %B %s %-80=|%4L |%-20,20f |%&user-date; \n")
;(setq gnus-summary-line-format (concat
;                                "%*%5{%U%R%z%}"
;                                "%4{\x49022%}"
;                                "%2{%-10&user-date;%}"
;                                "%4{\x49022%}"
;                                "%4{\x49022%}"
;                                "%2{ %}%(%-24,24n"
;                                "%4{\x49022%}"
;                                "%2{%5i%}"
;                                "%4{\x49022%}"
;                                "%2{%6k %}%)"
;                                "%4{\x49022%}"
;                                "%2{ %}%3{%B%}%1{%s%}\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the best! unicode threading pointers
;(when graf
; (setq gnus-sum-thread-tree-root "\x4912f ")
; (setq gnus-sum-thread-tree-single-indent "\x4912e ")
; (setq gnus-sum-thread-tree-leaf-with-other "\x4903c\x49020\x490fa ")
; (setq gnus-sum-thread-tree-vertical "\x49022")
; (setq gnus-sum-thread-tree-single-leaf "\x490b0\x49020\x490fa "))
;; here unicode threading ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(when graf
;  (setq gnus-group-highlight
;        '(((> unread 200) . my-group-face-1)
;          ((and (< level 3) (zerop unread)) . my-group-face-2)
;          ((< level 3) . my-group-face-3)
;          ((zerop unread) . my-group-face-4)
;          (t . my-group-face-5))))

;; always show MIME buttons
(setq gnus-inhibit-mime-unbuttonizing t)


;;;;;;
;; PGG
;;;;;;

(require 'pgg)
(autoload 'pgg-encrypt-region "pgg"
  "Encrypt the current region." t)
(autoload 'pgg-decrypt-region "pgg"
  "Decrypt the current region." t)
(autoload 'pgg-sign-region "pgg"
  "Sign the current region." t)
(autoload 'pgg-verify-region "pgg"
  "Verify the current region." t)
(autoload 'pgg-insert-key "pgg"
  "Insert the ASCII armored public key." t)
(autoload 'pgg-snarf-keys-region "pgg"
  "Import public keys in the current region." t)

;; verify/decrypt only if mml knows about the protocl used
(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)

;;Here we make button for the multipart
(setq gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed"))

;; Automcatically sign when sending mails
;(add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

;; Enough explicit settings
(setq pgg-passphrase-cache-expiry 300)
;(setq pgg-default-user-id prh::primary-key)

;; regexp of groups from which new messages are mime signed by default
;(setq my-sign-mime-group-regexp "^\\(INBOX.\\|\\)mail.work")

;; hook to setup message
;(defun my-mml-secure-message-sign-mime ()
;  (when (string-match
;	 my-sign-mime-group-regexp
;	 gnus-newsgroup-name)
;    (mml-secure-message-sign-smime)))

;; plug this into message-setup-hook
;(add-hook 'message-setup-hook 'my-mml-secure-message-sign-mime)

;; Automatic decryption/verification of gpg/pgp parts

;; Tells Gnus to inline the part
(eval-after-load "mm-decode"
  '(add-to-list 'mm-inlined-types "application/pgp$"))
;; Tells Gnus how to display the part when it is requested
(eval-after-load "mm-decode"
  '(add-to-list 'mm-inline-media-tests '("application/pgp$"
                                         mm-inline-text identity)))
;; Tell Gnus not to wait for a request, just display the thing
;; straight away.
(eval-after-load "mm-decode"
  '(add-to-list 'mm-automatic-display "application/pgp$"))
;; But don't display the signatures, please.
(eval-after-load "mm-decode"
  (quote (setq mm-automatic-display (remove "application/pgp-signature"
                                            mm-automatic-display))))

;;;;;;;
;; BBDB
;;;;;;;

(when (file-directory-p "~/var/bbdb/lisp")
  (add-to-list 'load-path "~/var/bbdb/lisp"))

(autoload 'bbdb-initialize "bbdb-autoloads")
;(require 'bbdb-autoloads)
;(require 'bbdb)
;(require 'bbdb-com)
(bbdb-initialize 'gnus 'message)

(add-hook 'message-setup-hook 'bbdb-define-all-aliases)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;; complete names in To:
(add-hook 'message-mode-hook
          (function (lambda()
                      (local-set-key (kbd "<tab>") 'bbdb-complete-name))))

(setq
 ;; allow cycling when completing already complete addresses
 bbdb-complete-name-allow-cycling t
 bbdb-canonicalize-redundant-nets-p t
 ;; save db wo/asking
; bbdb-offer-save 1
 ;; display layout
 bbdb-display-layout 'multi-line
 ;; number of lines in pop-up bbdb window
 bbdb-pop-up-target-lines 2
 bbdb-north-american-phone-numbers-p nil
 bbdb-default-country "Ukraine")



;;* Gnus soll im Article View eine dreigeteilte Ansicht haben:
;;* |----------------------------|
;;* |         Summary            |
;;* |----------------------------|
;;* |                   |        |
;;* |     Article       | *BBDB* |
;;* |___________________|________|
;(gnus-add-configuration
; '(article
;   (vertical 1.0                      ; erst ein verticaler Split
;             (summary 0.25 point)     ; in dessen 1. Teil kommt der
;                                      ; Summary Buffer
;             (horizontal 1.0          ; im 2. Teil einen horizontalen
;                                      ; Split
;                        (article 120) ; 1. Teil Article
;                                      ; Buffer
;                   ("*BBDB*" 1.0))))) ; 2. Teil *BBDB*
;                                      ; Buffer
;(setq bbdb-use-pop-up nil)

;;;;;;;;;;
;; Cite
;;;;;;;;;;

(autoload 'cite-cite "cite" "A simple cite function for Emacs" nil)
(setq message-cite-function 'cite-cite
      cite-remove-trailing-lines t
      cite-make-attribution-function #'prh-cite-attribution
      cite-prefix-regexp "[>]") ; don't allow fancy quoting

(defun prh-cite-attribution ()
  (let ((email (cite-get-header "email"))
        (name  (cite-get-header "name"))
        (date  (cite-get-header "date")))
    (if (and (null name) (null email))
        "An unnamed person wrote:\n\n"
      (if (null date)
          (concat (or name email) " wrote:\n\n")
        (concat "On " date ", " (or name email) " wrote:\n\n")))))

;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;

;; Moderating
(defun YSZ:award (name)
  (let ((group (message-fetch-reply-field "Newsgroups")))
        (cond
         ((string-match "ntu-kpi.comp.software" group)
          (progn
                (YSZ:award-insert-stuff name)))
         ((string-match "ntu-kpi.rec.games" group)
          (progn
                (YSZ:award-insert-stuff name)))
         (t
          (progn
                (message-goto-body)
                (insert "Ohrenel? V chujoy ehe-to nagrady stavit'??\n\n"))))))

(defun YSZ:award-insert-stuff (name)
  (insert "--=off\n")
  (insert moder-login "\n")
  (insert moder-pass "\n")
  (insert name)
  (insert "\n")
	(kill-line))

;; Now playing
(setq now-playing-file "~/.song")
(defun prh:now-playing ()
  "Returns the song now playing"
  (when (file-exists-p now-playing-file)
    (save-excursion
			(goto-char (point-max))
;      (let ((coding-system-for-read 'utf-16-le)) (insert-file-contents now-playing-file)))))
			(insert-file-contents now-playing-file)
			(goto-char (point-max))
			(newline))))

;; Random citation line
(setq random-sig-directory "~/.sigs")
(defun prh:random-cite ()
  "Adds to signature cite chosen randomly from `random-sig-directory'."
  (interactive)
  (when (file-exists-p random-sig-directory)
    (let* ((files (directory-files random-sig-directory t ""))
	   (file (nth (random (length files)) files)))
      (when file
        (save-excursion
		(goto-char (point-max))
          (insert-file-contents file))))))

;; This function is especially useful for message mode
(defun prh:intelligent-fill-paragraph ()
  "Tries to guess the fill-prefix based on the current line and fills the paragraph"
  (interactive)
  (beginning-of-line)
  (newline)
  (let ((a (point)))
    (end-of-line)
    (if (search-backward-regexp
         "^[ \t]*\\([>][ \t]*\\|[:#;|*][ \t]+\\|[a-zA-Z]+>[ \t]*\\)*"
         a t)
        (goto-char (match-end 0))
      (beginning-of-line))
    (set-fill-prefix)
    (fill-paragraph nil)
    (goto-char a)
    (backward-delete-char 1)
    (forward-paragraph)))


;;;;;;;;;;
;; Various hooks
;;;;;;;;;;

(add-hook
  'message-setup-hook
  '(lambda ()
     (let ((from-value (message-fetch-reply-field "From")))
       ;; moderator's award launcher
       (local-set-key [(control kp-add)] '(lambda () (interactive) (YSZ:award "+")))
       (local-set-key [(control kp-multiply)] '(lambda () (interactive) (YSZ:award "*")))
       (local-set-key [(control kp-subtract)] '(lambda () (interactive) (YSZ:award "-")))
       (local-set-key [(meta !)] '(lambda () (interactive) (YSZ:award "!")))
       )))

;; for normal working of C-c C-c, instead of award overrides
(add-hook
  'message-setup-hook
  '(lambda ()
     (local-set-key "\C-c\C-c" 'message-send-and-exit)))

(add-hook 'gnus-summary-mode-hook 'no-scroll-margin)

;; for now-playing and citation lines
;(add-hook 'message-setup-hook 'prh:random-cite)
;(add-hook 'message-setup-hook 'prh:now-playing)

(global-set-key (kbd "<f9>") (fun-for-bind toggle-buffer "*Group*"))

;;;;;;;;;;;;;;;;;;
;; Autocompilation
;;;;;;;;;;;;;;;;;;

(defun gnus-autocompile()
  "compile itself if ~/.gnus"
  (interactive)
  (if (string= (buffer-file-name) (concat default-directory ".gnus"))
      (byte-compile-file (buffer-file-name))))
;(add-hook 'after-save-hook 'gnus-autocompile())

;(setq smtpmail-debug-info t)
;(setq smtpmail-debug-verb t)
