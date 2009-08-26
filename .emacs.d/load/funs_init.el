(defalias 'qrr 'query-replace-regexp)

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
  (insert (format-time-string "%c")))

(defun whole-line ()
  "Returns list of two values - beginning of this line
and beginning of next line, for deleting/copying"
  (list (line-beginning-position) (line-beginning-position 2)))

(defun prh:copy-line ()
  "Save current line into Kill-Ring without marking the line "
  (buffer-substring (line-beginning-position) (line-end-position))
  )

(defun prh:check-newline (line)
  "Checks that line ends in newline. Adds it if not."
  (if (eql (aref line (1- (length line))) ?\n)
      line
    (concat line "\n")
  ))

(defun prh:cut-line ()
  "Kills current line"
  (let ((cutted-line (apply 'buffer-substring (whole-line))))
    (apply 'delete-region (whole-line))
    (prh:check-newline cutted-line)
    ))

(defun prh:count-lines (arg)
  "Count lines depending on arg.
If arg is positive, count from current position to end,
if negative, count from start to current position.
"
  (if (> arg 0)
      (count-lines (point) (point-max))
   (count-lines (point-min) (point))))

(defun prh:move-line (&optional arg)
  "Move current line.
Arg determines number of lines to skip, negative means move up."
  (interactive "p")
  (if (> (prh:count-lines arg) 0)
      (let ((prh:column (current-column)))
        (progn
          (or arg (setq arg 1))
          (forward-line 1)
          (transpose-lines arg)
          (forward-line -1)
          (move-to-column prh:column)))
    ))

(defun prh:move-line-down (&optional arg)
  "Move current line down. Optional ARG determines number of lines to skip"
  (interactive "p")
  (or arg (setq arg 1))
  (prh:move-line arg)
)

(defun prh:move-line-up (&optional arg)
  "Move current line up. Optional ARG determines number of lines to skip"
  (interactive "p")
  (or arg (setq arg 1))
  (prh:move-line (- arg))
)

(defun prh:duplicate-line (&optional arg)
  "Copy current line.
Arg determines number of lines to be created and direction."
  (interactive "p")
  (let ((cur-column (current-column))
        (cutted-line (prh:copy-line)))
    (progn
      (or arg (setq arg 1))
      (let ((tomove (if (< arg 0) (1+ arg) arg)))
        (end-of-line tomove))
      (newline)
      (insert cutted-line)
      (forward-line (- arg))
      (move-to-column cur-column)))
  )

(defun prh:duplicate-line-down (&optional arg)
  "Duplicate current line down. Optional ARG determines number of lines to skip"
  (interactive "p")
  (or arg (setq arg 1))
  (prh:duplicate-line arg)
)

(defun prh:duplicate-line-up (&optional arg)
  "Duplicate current line up. Optional ARG determines number of lines to skip"
  (interactive "p")
  (or arg (setq arg 1))
  (prh:duplicate-line (- arg))
)

(defun kill-region-or-word (&optional arg)
  "If region is active, kill it, backward kill word in other case."
  (interactive "p")
  (if (and transient-mark-mode mark-active)
      (kill-region (point) (mark))
    (backward-kill-word arg)
    ))

(defun toggle-file (desired-file)
  "Toggle buffer display of desired file."
  (when (file-exists-p desired-file)
    (if (and (buffer-file-name)
             (string= (expand-file-name desired-file)
                      (expand-file-name (buffer-file-name))))
        (bury-buffer)
      (find-file desired-file))))

(defun toggle-buffer (buffer-name)
  "Toggle display of desired buffer"
  (if (string= (buffer-name) buffer-name)
      (bury-buffer)
    (switch-to-buffer buffer-name t)))

(defun plist-maybe-get (keywords key)
  "If there is no \"key\" in \"keywords\", return nil.
Otherwise return value."
  (if (plist-member keywords key)
      (plist-get keywords key)
    nil))

(defun wc (region-min region-max)
  "Word count of whole buffer, if mark is active - of marked region"
  (interactive "r")
  (if (and transient-mark-mode mark-active)
      (message "Word count: %s" (how-many "\\w+" region-min region-max))
    (message "Word count: %s" (how-many "\\w+" (point-min) (point-max)))))

(defun sc (region-min region-max)
  "Symbol count of whole buffer, if mark is active - of marked region"
  (interactive "r")
  (if (and transient-mark-mode mark-active)
      (message "Symbol count: %s" (how-many "." region-min region-max))
    (message "Symbol count: %s" (how-many "." (point-min) (point-max)))))

(defalias 'cal 'calendar)

(defun dev-studio-beginning-of-line (arg)
  "Moves to beginning-of-line, or from there to the first non-whitespace character.

This takes a numeric prefix argument; when not 1, it behaves exactly like
\(move-beginning-of-line arg) instead."
  (interactive "p")
  (if (and (looking-at "^") (= arg 1)) (skip-chars-forward " \t") (move-beginning-of-line arg)))


(defvar django-closable-tags
  '("for" "block" "comment" "filter" "ifchanged" "ifequal"
    "ifnotequal" "spaceless" "if" "pyif" "with"))

(defvar django-tag-re
  (concat "{%\\s *\\(end\\)?\\("
          (mapconcat 'identity django-closable-tags "\\|")
          "\\)[^%]*%}"))

(defun django-find-open-tag ()
  (if (search-backward-regexp django-tag-re nil t)
      (if (match-string 1) ; If it's an end tag
          (if (not (string= (match-string 2) (django-find-open-tag)))
              (error "Unmatched Django tag")
            (django-find-open-tag))
        (match-string 2)) ; Otherwise, return the match
    nil))

(defun django-close-tag ()
  (interactive)
  (let ((open-tag (save-excursion (django-find-open-tag))))
    (if open-tag
        (insert "{% end" open-tag " %}")
      (error "Nothing to close"))))

(setq imenu-auto-rescan t)
(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a
   symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols
            (symbol-list)
            (when (listp symbol-list)
              (dolist (symbol symbol-list)
                (let ((name nil) (position nil))
                  (cond
                   ((and (listp symbol) (imenu--subalist-p symbol))
                    (addsymbols symbol))

                   ((listp symbol)
                    (setq name (car symbol))
                    (setq position (cdr symbol)))

                   ((stringp symbol)
                    (setq name symbol)
                    (setq position (get-text-property
                                    1 'org-imenu-marker symbol))))

                  (unless (or (null position) (null name))
                    (add-to-list 'symbol-names name)
                    (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol: " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

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
