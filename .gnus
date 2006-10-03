;; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gnus config
;; by Alexander Solovyov
;; piranha AT piranha.org.ua
;;
;; Special thank to all, who help me in creation, especially to:
;; Yuriy Sazonets <haze AT astral.ntu-kpi.kiev.ua>
;; Emacswiki.org ;)
;;
;; $Id: .gnus 11 2006-10-03 09:13:10Z piranha $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; graphics
;;;;;;;;;;;

(defconst graf
  (not (eq window-system 'nil))
  "Are we running window system?")

;;;;;;;;;;
;; Loading
;;;;;;;;;;
(require 'tc)
(require 'gnus-alias)
(require 'gnus-stat)
(require 'message-x)
(when (file-exists-p "~/.el/secrets.el")
  (load "~/.el/secrets.el"))

;;;;;;;;;;;
;; Language
;;;;;;;;;;;

(setq mm-body-charset-encoding-alist '((koi8-u . 8bit) ))
(setq gnus-group-posting-charset-alist nil)
;(push '("fido7.*" koi8-r t) gnus-group-posting-charset-alist)
;(push '("ntu-kpi.*" koi8-u t) gnus-group-posting-charset-alist)
(push '(".*" koi8-u t) gnus-group-posting-charset-alist)

;;;;;;;;;;;;
;; Extension config
;;;;;;;;;;;;

;; various

;; message-x
(setq message-x-body-function '(lambda () (interactive) (hippie-expand nil)))

;;;;;;;;;;
;; General
;;;;;;;;;;

;; auto word-wrap
(add-hook 'message-mode-hook
	(lambda ()
    (setq fill-column 79)
		(turn-on-auto-fill)))

(setq
  ;; be verbose
  gnus-novice-user nil
  ;; where move expired messages
  nnmail-expiry-target "nnmaildir:expired"
  ;; don't read whole active file (see help for add info)
  gnus-read-active-file nil
  ;; ask server for new newsgroups
  gnus-check-new-newsgroups 'ask-server
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
  gnus-signature-separator "^-- $"
	;; don't insert Cancel-Lock
  message-insert-canlock nil)

;; User settings
(setq
  user-name "piranha"
  user-full-name "Alexander Solovyov"
  user-mail-address "piranha@piranha.org.ua"
  gnus-local-domain "piranha.eth0.net.ua"
;  smtpmail-default-smtp-server "viii.ntu-kpi.kiev.ua"
;  smtpmail-local-domain "viii.ntu-kpi.kiev.ua"
  smtpmail-default-smtp-server "localhost"
  message-send-mail-function 'smtpmail-send-it
  smtpmail-auth-login-username "piranha")

(setq smtpmail-auth-credentials
      '(("localhost" 25 "piranha" "pft,bcm")))
;(setq smtpmail-debug-info t)

;; gnus-alias
(autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
(add-hook 'message-setup-hook 'gnus-alias-determine-identity)
(setq
 gnus-alias-identity-alist '(
      ("piranha"   "" "\"Alexander Solovyov\" <piranha@piranha.org.ua>" "" nil "" "")
      ("pviii" "" "\"Alexander Solovyov\" <piranha@viii.ntu-kpi.kiev.ua>" "" nil "" "")
      ("pgmail"  "" "\"Alexander Solovyov\" <alexander.solovyov@gmail.com>" "" nil "" "")
      ("moder-soft"   "" "\"Alexander Solovyov [Moderator]\" <piranha@viii.ntu-kpi.kiev.ua>" "" nil "" "~/.sign_moder-soft")
      ("moder-games"   "" "\"Alexander Solovyov [Moderator]\" <piranha@viii.ntu-kpi.kiev.ua>" "" nil "" "~/.sign_moder-games"))
 gnus-alias-default-identity "piranha")

(load-library "smtpmail")
(load-library "message")

(setq gnus-posting-styles
      '((".*"
         (name "Alexander Solovyov")
         (address "piranha@piranha.org.ua")
         (organization "Crazy Penguinz Crew")
         (signature-file "~/.signature"))
        ("ntu-kpi.*"
         ("X-Keywords" "parolcheg"))
        ("fido7.*"
         (name "Alexander Solovyov")
         (address "piranha@viii.ntu-kpi.kiev.ua")
         (signature-file "~/.sign_fido")
         ("Keywords:" "SUKANAXKEYWORD"))))

;; gnus-parameters
(setq gnus-parameters
      '((".*"
         (gnus-show-threads t)
         (gnus-use-scoring nil)
;         (display . all)
)))



;;;;;;;
;; News
;;;;;;;

(setq gnus-select-method '(nntp "news.ntu-kpi.kiev.ua"))
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; headers displayed
(setq gnus-visible-headers
 '("From:"
   "^Newsgroups:"
   "^Reply-To:"
   "^Subject:"
   "^Message-ID:"
   "^Date:"
   "^To:"
   "^Cc:"
   "^Posted-To:"
   "^Mail-Copies-To:"
   "^Resent-From:"
   "^X-Sent:"
   "^User-Agent:"
   "^Content-Type:"
   "^X-Newsreader:"
   "^NNTP-Posting-Host:"))

;; message forwarding
(setq
   message-forward-as-mime nil
   message-forward-ignored-headers "^Content-Transfer-Encoding:\\|^X-\\|^References\\|^Xref\\|^In-Reply-To\\|^Received\\|^NNTP\\|^Return-Path\\|^Sender\\|^Approved\\|^User-Agent\\|^List-\\|^Error-To\\|^FL-\\|^Precedence\\|^Gnus-\\|^Path\\|^Cancel-Lock\\|^Face")

(when graf
  (setq gnus-group-highlight
        '(((> unread 200) . my-group-face-1)
          ((and (< level 3) (zerop unread)) . my-group-face-2)
          ((< level 3) . my-group-face-3)
          ((zerop unread) . my-group-face-4)
          (t . my-group-face-5))))

;; always show MIME buttons
(setq gnus-inhibit-mime-unbuttonizing t)

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
;(setq gnus-summary-line-format
; ":%U%R | %B %s %-105=|%4L |%-30,30f |%&user-date; \n")
(when graf
  (setq gnus-summary-line-format (concat
                                  "%*%5{%U%R%z%}"
                                  "%4{\x49022%}"
                                  "%2{%-10&user-date;%}"
                                  "%4{\x49022%}"
                                  "%4{\x49022%}"
                                  "%2{ %}%(%-24,24n"
                                  "%4{\x49022%}"
                                  "%2{%5i%}"
                                  "%4{\x49022%}"
                                  "%2{%6k %}%)"
                                  "%4{\x49022%}"
                                  "%2{ %}%3{%B%}%1{%s%}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the best! unicode threading pointers
(when graf
 (setq gnus-sum-thread-tree-root "\x4912f ")
 (setq gnus-sum-thread-tree-single-indent "\x4912e ")
 (setq gnus-sum-thread-tree-leaf-with-other "\x4903c\x49020\x490fa ")
 (setq gnus-sum-thread-tree-vertical "\x49022")
 (setq gnus-sum-thread-tree-single-leaf "\x490b0\x49020\x490fa "))
;; here unicode threading ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gnus-summary-same-subject "")

;;;;;;;;
;; Faces
(when graf
      (setq custom-background-mode 'light)
      (defface my-group-face-1
        '((t (:foreground "Red" :bold t))) "First group face")
      (defface my-group-face-2
        '((t (:foreground "DarkSeaGreen4" :bold t))) "Second group face")
      (defface my-group-face-3
        '((t (:foreground "Green4" :bold t))) "Third group face")
      (defface my-group-face-4
        '((t (:foreground "CornflowerBlue" :bold t))) "Fourth group face")
      (defface my-group-face-5
         '((t (:foreground "DeepSkyBlue" :bold t))) "Fifth group face")

(copy-face 'default 'mysubject)
(setq gnus-face-1 'mysubject)

(copy-face 'default 'mytime)
(set-face-foreground 'mytime "green3")
(setq gnus-face-2 'mytime)

(copy-face 'default 'mythreads)
(set-face-foreground 'mythreads "indianred1")
(setq gnus-face-3 'mythreads)

(copy-face 'default 'mygrey)
(set-face-foreground 'mygrey "grey")
(setq gnus-face-4 'mygrey)

(copy-face 'default 'myblack)
(set-face-foreground 'myblack "grey60")
(setq gnus-face-5 'myblack)

(copy-face 'default 'mybiggernumbers)
(set-face-foreground 'mybiggernumbers "indianred4")
(setq gnus-face-6 'mybiggernumbers)

(set-face-foreground 'gnus-summary-normal-ancient-face "LightSteelBlue")

;; to view my own postings
(require 'gnus-sum)
(defface dz-gnus-own-posting-face nil
  "Use this face to display own postings in Summary Buffer")
(copy-face 'gnus-summary-high-unread-face 'dz-gnus-own-posting-face)
(set-face-foreground 'dz-gnus-own-posting-face "aquamarine")

(defface dz-gnus-direct-fup-face nil
  "Use this face to display direct fups to my postings.")
(copy-face 'gnus-summary-high-unread-face 'dz-gnus-dir-fup-face)
(set-face-foreground 'dz-gnus-direct-fup-face "yellow")

(defface dz-gnus-indirect-fup-face nil
  "Use this face to display indirect fups to my postings")
(copy-face 'gnus-summary-high-unread-face 'dz-gnus-indir-fup-face)
(set-face-foreground 'dz-gnus-indirect-fup-face "lightgreen")
)
;; end of faces
;;;;;;;;;;;;;;;


;;;;;;;
;; Mail
;;;;;;;

(setq gnus-secondary-select-methods '((nnmaildir "")))
;(setq mail-sources
;           '((pop :server "localhost"
;                  :port "pop3"
;                  :user "piranha"
;                  :password "parolcheg"
;)))

;; Mail sorting
;(setq nnmail-split-methods '(
;     ("hostels" "^\\(To\\|From\\|Cc\\):.*hostels.*@hosix\\.ntu-kpi\\.kiev\\.ua.*")
;     ("humor" "^\\(To\\|From\\|Cc\\):.*humor@xcp\\.kiev\\.ua.*")
;     ("moderatorials" "^\\(To\\|From\\|Cc\\):.*comp.software@library\\.ntu-kpi\\.kiev\\.ua.*")
;     ("roka" "^\\(From\\):.*roka@.*")
;     ("spam" "^\\(Subject\\):.*SPAM.*")
;     ("root" "^\\(To\\|From\\|Cc\\):.*root@.*")
;     ("murkt" "^\\(From\\):.*murkt@eth0\\.org\\.ua.*")
;     ("security" "^\\(From\\):.*daily@security\\.nnov\\.ru.*")
;     ("hostels" "^\\(To\\):.*hostels@ntu-kpi\\.kiev\\.ua.*")
;     ("news-talk" "^\\(To\\|Cc\\):.*talk@news.ntu-kpi.kiev.ua.*")
;     ("anime" "^\\(To\\):.*anime_kpi@yahoogroups.com.*")
;     ("inbox" "")))

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
;				("From" ".*Nikita Gubenko \\[CoModerator\\].*" "moder")
;        ("List-Id" ".*c-p-c.googlegroups.com.*" "cpc")
;        ("List-Id" ".*newstalk.news.ntu-kpi.kiev.ua.*" "news-talk")
;				("List-Id" ".*exim-users.exim.org.ua.*" "exim")
;				("List-Id" ".*eth0.googlegroups.com.*" "eth0")
;				("List-Id" ".*users.lists.eth0.net.ua.*" "eth0-public")
;				("List-Id" ".*sudoers.lists.eth0.net.ua.*" "eth0-sudoers")
;				("List-Id" ".*ik22.lists.eth0.net.ua.*" "ik22")
;				("From" ".*@livejournal.com.*" "lj")
;				("From" ".*root@eth0.net.ua.*" "root")
;				("Subject" ".*\\*\\*\\*SPAM\\*\\*\\*.*" "spam")
;        (any ".*" (: prh:mail-date "inbox"))
;        ))


;; save messages
(setq gnus-message-archive-group '(
    (if (message-news-p) (concat "maildir:sent-news" (format-time-string ".%Y.%m" (current-time)))
      (concat "maildir:sent-mail" (format-time-string ".%Y.%m" (current-time))))))

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

;(require 'bbdb-autoloads)
;(require 'bbdb)
;(require 'bbdb-com)
;(bbdb-initialize 'gnus 'message)

;; complete names in To:
;(add-hook 'message-mode-hook
;   (function (lambda()
;      (local-set-key (kbd "<tab>") 'bbdb-complete-name)
;   )))

;(setq
;  ;; allow cycling when completing already complete addresses
;  bbdb-complete-name-allow-cycling t
;  ;; see C-h, v for description
;;  bbdb-canonicalize-redundant-nets-p t
;  ;; save db wo/asking
;  bbdb-offer-save 1
;  ;; display layout
;  bbdb-display-layout 'multi-line
;  ;; number of lines in pop-up bbdb window
;  bbdb-pop-up-target-lines 2
;  ;; C-h, v
;  bbdb-north-american-phone-numbers-p nil)

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
;; Trivial Cite
;;;;;;;;;;

(autoload 'trivial-cite "tc" t t)
(add-hook 'mail-citation-hook 'trivial-cite)
(setq
  message-cite-function 'trivial-cite
  tc-time-format "%e.%m at %H:%M"
  tc-make-attribution 'tc-piranha-attribution
	tc-remove-signature "^\\(-- \\|_______________________________________________\\)$"
	tc-guess-cite-marks nil
  ;; don't rearrange quoted text
  tc-fill-column nil)

;; attribution
(defun tc-piranha-attribution ()
	      "Produce attribution string, using the real name, as piranha wish. :)"
  (let ((date (assoc "date" tc-strings-list))
	(email (assoc "email-addr" tc-strings-list))
        (name (assoc "real-name" tc-strings-list)))
    (if (and (null name) (null email))
	"An unnamed person wrote:\n\n"
      (if (null date)
	  (concat "Ave, " (cdr (or name email)) ". You wrote:\n\n")
        (concat "Ave, " (cdr (or name email)) ", on " (cdr date) " you wrote:\n\n")))))

;; blank quoted lines removal
(defun rs-message-remove-blank-cited-lines (&optional remove)
   "Remove cited lines containing only blanks.
 If REMOVE is non-nil, remove newlines, too."
   ;; Idea by Karl Pla"sterer,
   ;; see <m3adiiup8o.fsf@hamster.pflaesterer.de> ff.
   (interactive "P")
   (let ((citexp
          (concat
    "^\\("
    tc-citation-string
    "\\)+ *$"
    (if remove "\n" ""))))
     (save-excursion
       (while (re-search-forward citexp nil t)
         (replace-match "")))))
(add-hook 'tc-post-hook 'rs-message-remove-blank-cited-lines 't)


;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;

;; Moderatoring
(defun YSZ:award (name)
  (let ((group (message-fetch-reply-field "Newsgroups")))
        (cond
         ((string-match "ntu-kpi.comp.software" group)
          (progn
                (gnus-alias-use-identity "moder-soft")
                (YSZ:award-insert-stuff name)))
         ((string-match "ntu-kpi.rec.games" group)
          (progn
                (gnus-alias-use-identity "moder-games")
                (YSZ:award-insert-stuff name)))
         (t
          (progn
                (message-goto-body)
                (insert "Ohrenel? V chujoy ehe-to nagrady stavit'??\n\n")) ) ) ) )

(defun YSZ:award-insert-stuff (name)
  (insert "--=off\n")
  (insert moder-login "\n")
  (insert moder-pass "\n")
  (insert name)
  (insert "\n")
	(kill-line)
  )


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
			(newline)
		)))

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

;; for now-playing and citation lines
(add-hook 'message-setup-hook 'prh:random-cite)
(add-hook 'message-setup-hook 'prh:now-playing)

;; hl-line-mode
(add-hook 'gnus-summary-mode-hook 'my-setup-hl-line)
;(add-hook 'gnus-group-mode-hook 'my-setup-hl-line)
(defun my-setup-hl-line ()
 (hl-line-mode 1)
;  (setq cursor-type nil) ; Comment this out, if you want the cursor to
			 ; stay visible.
; (set-face-foreground 'highlight "Green")
 (copy-face 'default 'highlight)
 (set-face-background 'highlight "DarkGreen")
)


;;;;;;;;;;;;;;;;;;
;; Autocompilation
;;;;;;;;;;;;;;;;;;

(defun gnus-autocompile()
  "compile itself if ~/.gnus"
  (interactive)
  (if (string= (buffer-file-name) (concat default-directory ".gnus"))
      (byte-compile-file (buffer-file-name))))
(add-hook 'after-save-hook 'gnus-autocompile())

;(setq smtpmail-debug-info t)
;(setq smtpmail-debug-verb t)
