(when (file-directory-p "~/var/org/lisp")
  (add-to-list 'load-path "~/var/org/lisp"))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-log-done t
      org-return-follows-link t
      org-hide-emphasis-markers t
      org-hide-leading-stars t
      org-odd-levels-only t
      org-agenda-start-on-weekday nil
      org-agenda-ndays 7
      org-agenda-skip-scheduled-if-done t)

(autoload 'appt-convert-time "appt" "Convert time to minutes from midnight.")
(autoload 'org-agenda-get-day-entries "org-agenda" "")

(defun sacha/org-agenda-load (match)
  "Can be included in `org-agenda-custom-commands'."
  (let ((inhibit-read-only t)
        (time (sacha/org-calculate-free-time
               ;; today
               (calendar-gregorian-from-absolute org-starting-day)
               ;; now if today AND after starting time, else start of day
               (if (= org-starting-day
                      (time-to-days (current-time)))
                   (max
                    (let* ((now (decode-time))
                           (cur-hour (nth 2 now))
                           (cur-min (nth 1 now)))
                      (+ (* cur-hour 60) cur-min))
                    (let ((start (car (elt org-agenda-time-grid 2))))
                      (+ (* (/ start 100) 60) (% start 100))))
                 (let ((start (car (elt org-agenda-time-grid 2))))
                   (+ (* (/ start 100) 60) (% start 100))))
               ;; until the last time in my time grid
               (let ((last (car (last (elt org-agenda-time-grid 2)))))
                 (+ (* (/ last 100) 60) (% last 100))))))
    (goto-char (point-max))
    (insert (format
             "%.1f%% load: %d minutes to be scheduled, %d minutes free, %d minutes gap"
             (/ (car time) (* .01 (cdr time)))
             (car time)
             (cdr time)
             (- (cdr time) (car time))))))

(defun sacha/org-calculate-free-time (date start-time end-of-day)
  "Return a cons cell of the form (TASK-TIME . FREE-TIME) for DATE, given START-TIME and END-OF-DAY.
DATE is a list of the form (MONTH DAY YEAR).
START-TIME and END-OF-DAY are the number of minutes past midnight."
  (save-window-excursion
    (let ((files org-agenda-files)
          (total-unscheduled 0)
          (total-gap 0)
          file
          rtn
          rtnall
          entry
          (last-timestamp start-time)
          scheduled-entries)
      (while (setq file (car files))
        (catch 'nextfile
          (org-check-agenda-file file)
          (setq rtn (org-agenda-get-day-entries file date :scheduled :timestamp))
          (setq rtnall (append rtnall rtn)))
        (setq files (cdr files)))
      ;; For each item on the list
      (while (setq entry (car rtnall))
        (let ((time (get-text-property 1 'time entry)))
          (cond
           ((and time (string-match "\\([^-]+\\)-\\([^-]+\\)" time))
            (setq scheduled-entries (cons (cons
                                           (save-match-data (appt-convert-time (match-string 1 time)))
                                           (save-match-data (appt-convert-time (match-string 2 time))))
                                          scheduled-entries)))
           ((and time
                 (string-match "\\([^-]+\\)\\.+" time)
                 (string-match "^[A-Z]+ \\(\\[#[A-Z]\\] \\)?\\([0-9]+\\)" (get-text-property 1 'txt entry)))
            (setq scheduled-entries
                  (let ((start (and (string-match "\\([^-]+\\)\\.+" time)
                                    (appt-convert-time (match-string 2 time)))))
                    (cons (cons start
                                (and (string-match "^[A-Z]+ \\(\\[#[A-Z]\\] \\)?\\([0-9]+\\)" (get-text-property 1 'txt entry))
                                     (+ start (string-to-number (match-string 2 (get-text-property 1 'txt entry))))))
                          scheduled-entries))))
           ((string-match "^[A-Z]+ \\(\\[#[A-Z]\\] \\)?\\([0-9]+\\)" (get-text-property 1 'txt entry))
            (setq total-unscheduled (+ (string-to-number
                                        (match-string 2 (get-text-property 1 'txt entry)))
                                       total-unscheduled)))))
        (setq rtnall (cdr rtnall)))
      ;; Sort the scheduled entries by time
      (setq scheduled-entries (sort scheduled-entries (lambda (a b) (< (car a) (car b)))))

      (while scheduled-entries
        (let ((start (car (car scheduled-entries)))
              (end (cdr (car scheduled-entries))))
          (cond
           ;; are we in the middle of this timeslot?
           ((and (>= last-timestamp start)
                 (<= last-timestamp end))
            ;; move timestamp later, no change to time
            (setq last-timestamp end))
           ;; are we completely before this timeslot?
           ((< last-timestamp start)
            ;; add gap to total, skip to the end
            (setq total-gap (+ (- start last-timestamp) total-gap))
            (setq last-timestamp end)))
          (setq scheduled-entries (cdr scheduled-entries))))
      (if (< last-timestamp end-of-day)
          (setq total-gap (+ (- end-of-day last-timestamp) total-gap)))
      (cons total-unscheduled total-gap))))

(defun sacha/org-clock-in-if-starting ()
  "Clock in when the task is marked STARTED."
  (when (and (string= state "STARTED")
             (not (string= last-state state)))
    (org-clock-in)))
(add-hook 'org-after-todo-state-change-hook
          'sacha/org-clock-in-if-starting)
(defadvice org-clock-in (after sacha activate)
  "Set this task's status to 'STARTED'."
  (org-todo "STARTED"))

(defun sacha/org-clock-out-if-waiting ()
  "Clock out when the task is marked WAITING."
  (when (and (string= state "WAITING")
             (not (string= last-state state)))
    (org-clock-out)))
(add-hook 'org-after-todo-state-change-hook
          'sacha/org-clock-out-if-waiting)


(setq org-agenda-custom-commands
 '(("a" "My agenda" ((org-agenda-list nil nil 1)
                     (sacha/org-agenda-load)
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
(global-set-key (kbd "<f2>") (fun-for-bind toggle-file "~/org/musicx.org"))
(global-set-key (kbd "<f3>") (fun-for-bind toggle-file "~/org/mh.org"))

(when (file-directory-p "~/var/remember")
  (add-to-list 'load-path "~/var/remember"))

(autoload 'remember "remember" "Remember note")
(global-set-key (kbd "M-R") 'remember)
(eval-after-load "remember"
  '(progn
     (require 'org-remember)
     (add-hook 'remember-mode-hook 'org-remember-apply-template)
     (setq
      org-remember-store-without-prompt t
      org-remember-templates '((116 "* TODO %?\n  %u" "~/org/life.org" "Inbox")
                                (110 "* %u %?" "~/org/life.org" "Notes"))
      remember-annotation-functions '(org-remember-annotation)
      remember-handler-functions '(org-remember-handler))))
