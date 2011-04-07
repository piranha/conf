(when (file-directory-p "~/var/org/lisp")
  (add-to-list 'load-path "~/var/org/lisp"))

(setq
 ;; Set to the location of your Org files on your local system
 org-directory "~/org"
 ;; Set to the name of the file where new notes will be stored
 org-mobile-inbox-for-pull "~/org/flagged.org"
 ;; Set to <your Dropbox root directory>/MobileOrg.
 org-mobile-directory "~/Dropbox/MobileOrg"
 org-agenda-filter nil
 org-drawers-for-agenda nil
 org-tag-alist-for-agenda nil)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-log-done t
      org-return-follows-link t
      org-hide-emphasis-markers t
      org-hide-leading-stars t
      org-odd-levels-only t
      org-agenda-start-on-weekday nil
      org-agenda-ndays 7
      org-agenda-skip-scheduled-if-done t
      org-table-export-default-format "orgtbl-to-csv")

(autoload 'appt-convert-time "appt" "Convert time to minutes from midnight.")
(autoload 'org-agenda-get-day-entries "org-agenda" "")

(setq org-agenda-custom-commands
 '(("a" "My agenda" ((org-agenda-list nil nil 1)
                     (todo "WAITING")))
   ("c" todo "DONE|CANCELLED" nil)
   ("w" todo "WAITING" nil)
   ("W" agenda "" ((org-agenda-ndays 21)))
   ("A" agenda ""
    ((org-agenda-skip-function
      (lambda nil
        (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
     (org-agenda-ndays 1)
     (org-agenda-overriding-header "Today's Priority #A tasks: ")))
   ("u" alltodo ""
    ((org-agenda-skip-function
      (lambda nil
        (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                  (quote regexp) "<[^>\n]+>")))
     (org-agenda-overriding-header "Unscheduled TODO entries: ")))))

(defun prh/org-open-at-point (&optional not-in-emacs)
  "Open link at point, by default in emacs. Based on `org-open-at-point`."
  (interactive "P")
  (org-open-at-point (not not-in-emacs)))

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-,") (fun-for-bind bs--show-with-configuration "org"))
     (define-key org-mouse-map [(return)] 'prh/org-open-at-point)))


(global-set-key (kbd "<f1>") (fun-for-bind toggle-file "~/org/life.org"))

(autoload 'remember "remember" "Remember note")
(global-set-key (kbd "M-R") 'remember)
(eval-after-load "remember"
  '(progn
     (require 'org-remember)
     (add-hook 'remember-mode-hook 'org-remember-apply-template)
     (setq
      org-remember-store-without-prompt t
      org-remember-templates '(;(116 "* TODO %?\n  %u" "~/org/life.org" "Inbox")
                                (110 "* %u %?" "~/org/life.org" "Inbox"))
      remember-annotation-functions '(org-remember-annotation)
      remember-handler-functions '(org-remember-handler))))
