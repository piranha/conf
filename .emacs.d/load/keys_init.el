(defmacro fun-for-bind (func &rest args)
  "Returns a symbol of an anonymous interactive function,
suitable for binding to keys."
  `(lambda () (interactive) (,func ,@args)))

(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-,") 'bs-show)
(global-set-key (kbd "C-.") 'iswitchb-buffer)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-x C-r") 'query-replace-regexp)
(global-set-key (kbd "C-x C-a") 'imenu)
(global-set-key (kbd "M-.") 'find-tag)

(global-set-key (kbd "C-x C-d") 'dired-jump)

(global-set-key (kbd "<f5>") 'kmacro-end-and-call-macro)

(global-set-key (kbd "C-c k") (fun-for-bind kill-buffer nil))
(global-set-key (kbd "C-M-l") (fun-for-bind switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-M-z") (lambda (&optional arg char)
                                (interactive "p\ncZap backward to char: ")
                                (zap-to-char (- arg) char)))
(global-set-key (kbd "C-M-y") (lambda (&optional arg)
                                (interactive "*p")
                                (yank-pop (- arg))))
(global-set-key (kbd "M-<up>") 'prh:move-line-up)
(global-set-key (kbd "M-<down>") 'prh:move-line-down)
(global-set-key (kbd "C-M-<up>") 'prh:duplicate-line-up)
(global-set-key (kbd "C-M-<down>") 'prh:duplicate-line-down)

(when win
  (global-set-key (kbd "C-<f12>") (fun-for-bind w32-send-sys-command 61488 nil))
  )
