(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-,") 'bs-show)
(global-set-key (kbd "C-.") 'ido-switch-buffer)
(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-x C-r") 'query-replace-regexp)
(global-set-key (kbd "M-A") 'ido-goto-symbol)
(global-set-key (kbd "M-.") 'find-tag)
(global-set-key (kbd "C-<f12>") 'toggle-current-window-dedication)
(global-set-key (kbd "C-S-k") (lambda () (interactive) (kill-line 0)))

(global-set-key (kbd "C-x C-d") 'dired-jump)

(global-set-key (kbd "C-c k") (fun-for-bind kill-buffer nil))
(global-set-key (kbd "C-M-l") (fun-for-bind switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-M-z") (lambda (&optional arg char)
                                (interactive "p\ncZap backward to char: ")
                                (zap-to-char (- arg) char)))
(global-set-key (kbd "M-<up>") 'prh:move-line-up)
(global-set-key (kbd "M-<down>") 'prh:move-line-down)
(global-set-key (kbd "C-M-<up>") 'prh:duplicate-line-up)
(global-set-key (kbd "C-M-<down>") 'prh:duplicate-line-down)
(global-set-key (kbd "C-a") 'dev-studio-beginning-of-line)
(global-set-key (kbd "<home>") 'dev-studio-beginning-of-line)

(global-set-key (kbd "M-Y") (lambda (&optional arg)
                              (interactive "*p")
                              (yank-pop (- arg))))
(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))
(global-set-key (kbd "C-M-y") 'kill-ring-search)


(global-set-key (kbd "<f5>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "<f11>") 'jabber-activity-switch-to)

;; windows
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)

(if (fboundp 'w32-send-sys-command)
    (progn
      (global-set-key (kbd "C-<f12>") ; maximize
                      (fun-for-bind w32-send-sys-command #xf030 nil))
      (global-set-key (kbd "C-<f11>") ; restore original size
                      (fun-for-bind w32-send-sys-command #xf120 nil))))
