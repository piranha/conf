(defalias 'qrr 'query-replace-regexp)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'rof 'recentf-open-files)

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

(defalias 'cal 'calendar)

(defun beginning-of-line-dwim (arg)
  "Moves to beginning-of-line, or from there to the first non-whitespace character.

This takes a numeric prefix argument; when not 1, it behaves exactly like
\(move-beginning-of-line arg) instead."
  (interactive "p")
  (if (and (looking-at "^") (= arg 1)) (skip-chars-forward " \t") (move-beginning-of-line arg)))

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

(defun djcb-snip (b e summ)
  "remove selected lines, and replace it with [snip:summary (n lines)]"
  (interactive "r\nsSummary:")
  (let ((n (count-lines b e)))
    (delete-region b e)
    (insert (format "[snip%s (%d line%s)]"
                    (if (= 0 (length summ)) "" (concat ": " summ))
                    n
                    (if (= 1 n) "" "s")))))

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

(defun prh:isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun prh:isearch-yank-word-hook ()
  (when (equal this-command 'prh:isearch-word-at-point)
    (let ((string (concat "\\_<"
                          (buffer-substring-no-properties
                           (progn (skip-syntax-backward "w_") (point))
                           (progn (skip-syntax-forward "w_") (point)))
                          "\\_>")))
      (if (and isearch-case-fold-search
               (eq 'not-yanks search-upper-case))
          (setq string (downcase string)))
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))

(add-hook 'isearch-mode-hook 'prh:isearch-yank-word-hook)

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
