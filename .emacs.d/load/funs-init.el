(defun no-scroll-margin ()
  "Set scroll-margin to 0 buffer-locally"
  (interactive)
  (set (make-local-variable 'scroll-margin) 0))

(defun insert-date (format)
  "Wrapper around format-time-string."
  (interactive "MFormat: ")
  (insert (format-time-string format)))

(defun insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%F %T")))

(defun prh:copy-line ()
  "Save current line into Kill-Ring without marking the line "
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun prh:move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun prh:move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun toggle-file (desired-file)
  "Toggle buffer display of desired file."
  (when (file-exists-p desired-file)
    (if (and (buffer-file-name)
             (string= (expand-file-name desired-file)
                      (expand-file-name (buffer-file-name))))
        (bury-buffer)
      (find-file desired-file))))

(defun toggle-buffer (buffer-name)
  "Toggle display of desired buffer."
  (if (string= (buffer-name) buffer-name)
      (bury-buffer)
    (switch-to-buffer buffer-name t)))

(defun plist-maybe-get (keywords key)
  "If there is no \"key\" in \"keywords\", return nil.
Otherwise return value."
  (if (plist-member keywords key)
      (plist-get keywords key)
    nil))

(defalias 'cal 'calendar)

(defun beginning-of-line-dwim (arg)
  "Moves to beginning-of-line, or from there to the first non-whitespace character.

This takes a numeric prefix argument; when not 1, it behaves exactly like
\(move-beginning-of-line arg) instead."
  (interactive "p")
  (if (and (looking-at "^") (= arg 1))
      (skip-chars-forward " \t")
    (move-beginning-of-line arg)))

(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(defun newline-maybe-indent ()
  "Like newline-and-indent, but doesn't indent if the previous line is blank"
  (interactive "*")
  (if (= (line-beginning-position) (line-end-position))
      (newline)
    (newline-and-indent)))

(defun time-to-number (time)
  "Convert time from format 9:30 to number"
  (let ((time (if (string-match ":" time)
                  (mapcar 'string-to-number (split-string time ":"))
                `(,(string-to-number time) 0))))
    (+ (car time) (/ (float (cadr time)) 60))))

(defun copy-path-to-clipboard ()
  "Copy the current file's path to the clipboard.
  If the current buffer has no file, copy the buffer's default directory."
  (interactive)
  (let ((path (file-truename (or (buffer-file-name) default-directory))))
    (kill-new path)
    (message "%s" path)))

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method 'cyrillic-jcuken)

(defun html-end-of-line ()
  "If there is an HTML tag at the end of the line, then go to start of tag.
   Otherwise go to the real end of the line."
  (interactive)
  (if (or (looking-at ".*>$") ; if we're on a line that ends with a tag
          (and (= (char-before) 62)
               (= (point) (save-excursion
                            (end-of-line)
                            (point))))) ; or we're at the end of a line
                                        ; with a tag
      (let ((where-now (point)))
        (narrow-to-region
         (save-excursion
           (beginning-of-line)
           (point))
         (save-excursion
           (end-of-line)
           (point)))
        (end-of-line)
        (re-search-backward "<" nil t)
        (if (= (point) where-now)
            (end-of-line))
        (widen))
    (end-of-line)))

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))


;;; perfomance timing

(defvar time-each-command-start nil)

(defun time-each-command-pre ()
  (setq time-each-command-start (current-time)))

(defun time-each-command-post ()
  (message
   "Time to run %S was %sms"
   this-command
   (* 1000 (float-time (time-subtract
                        (current-time)
                        time-each-command-start)))))

(define-minor-mode time-each-command-mode
  "Print in minibuffer how long each command takes to execute."
  :global t
  (if time-each-command-mode
      (progn
        (add-hook 'pre-command-hook #'time-each-command-pre)
        (add-hook 'post-command-hook #'time-each-command-post))
    (remove-hook 'pre-command-hook #'time-each-command-pre)
    (remove-hook 'post-command-hook #'time-each-command-post)))


;;; diff

(defun diff-last-two-kills (&optional ediff?)
  "Diff last couple of things in the kill-ring. With prefix open ediff."
  (interactive "P")
  (let* ((old "/tmp/old-kill")
         (new "/tmp/new-kill")
         (prev-ediff-quit-hook (if (fboundp 'ediff-quit-hook)
                                   (symbol-value 'ediff-quit-hook))))
    (cl-flet ((kill-temps ()
                          (dolist (f (list old new))
                            (kill-buffer (find-buffer-visiting f)))
                          (if prev-ediff-quit-hook
                              (setq ediff-quit-hook prev-ediff-quit-hook))))
      (with-temp-file new
        (insert (current-kill 0 t)))
      (with-temp-file old
        (insert (current-kill 1 t)))
      (if ediff?
          (progn
            (add-hook 'ediff-quit-hook #'kill-temps)
            (ediff old new))
        (diff old new "-u" t)))))


;;; kasta changelog

(defun newlog ()
  (interactive)
  (let ((path (concat (projectile-project-root)
                      "changelog.d/"
                      (format-time-string "%s"))))
    (write-region "" nil path)
    (find-file path)
    (add-hook 'after-save-hook
              (lambda () (shell-command (concat "git add " (buffer-file-name))))
              nil t)))

;;; utils

(defun dom-tag-value (search-attr search-value)
  (require 'dom)
  (thread-first (libxml-parse-html-region (point-min) (point-max))
    (dom-search #'(lambda (node) (string= (dom-attr node search-attr) search-value)))
    car
    (dom-attr 'value)))


(defun read-export-value (filename var)
  "Read the value of a variable from an export file."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward (concat "^export " var "=\\([^[:space:]]+\\)") nil t)
        (match-string 1)))))
