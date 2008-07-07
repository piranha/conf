(setq
 eshell-cmpl-cycle-completions nil
 eshell-buffer-shorthand t
 eshell-output-filter-functions '(eshell-handle-control-codes eshell-watch-for-password-prompt eshell-postoutput-scroll-to-bottom)
 eshell-scroll-show-maximum-output t
 eshell-scroll-to-bottom-on-output nil
 eshell-scroll-to-bottom-on-input 'this
)

;; scroll to bottom for eshell

(eval-after-load "eshell"
  '(progn

     (defun eshell-scroll-to-bottom (window display-start)
       (if (and window (window-live-p window))
           (let ((resize-mini-windows nil))
             (save-selected-window
               (select-window window)
               (save-restriction
                 (widen)
                 (when (> (point) eshell-last-output-start) ; we're editing a line. Scroll.
                   (save-excursion
                     (recenter -1)
                     (sit-for 0))))))))

     (defun eshell-add-scroll-to-bottom ()
       (interactive)
       (add-hook 'window-scroll-functions 'eshell-scroll-to-bottom nil t))

     (add-hook 'eshell-mode-hook 'eshell-add-scroll-to-bottom)

     (defun eshell/e (&rest args)
       (if (null args)
           (bury-buffer)
         (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

     (defsubst eshell/ls (&rest args)
       "An alias version of `eshell-do-ls'."
       (let ((insert-func 'eshell-buffered-print)
             (error-func 'eshell-error)
             (flush-func 'eshell-flush)
             (args-plus (append
                         (cond ((not (eq (car (aref eshell-current-handles 1)) t))
                                (list "-1")))
                         args)))
         (eshell-do-ls args-plus)))

     (defun eshell-maybe-bol ()
       (interactive)
       (let ((p (point)))
         (eshell-bol)
         (if (= p (point))
             (beginning-of-line))))

     ;; eshell-mode-map is buffer-local
     (add-hook 'eshell-mode-hook (lambda ()
                                   (define-key eshell-mode-map (kbd "C-a") 'eshell-maybe-bol)
                                   (define-key eshell-mode-map (kbd "<home>") 'eshell-maybe-bol)))))
