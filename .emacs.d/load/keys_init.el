(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-,") 'bs-show)
(global-set-key (kbd "C-.") 'ido-switch-buffer)
(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-x C-r") 'query-replace-regexp)

(autoload 'idomenu "idomenu" nil t)
(global-set-key (kbd "M-A") 'idomenu)

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
(global-set-key (kbd "C-a") 'beginning-of-line-dwim)
(global-set-key (kbd "<home>") 'beginning-of-line-dwim)

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
(global-set-key (kbd "M-`") 'other-frame)
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

(autoload 'piu "piu" "paste buffer or region")
(global-set-key (kbd "C-x p") 'piu)

(dolist (nose-func '(nosetests-all nosetests-module nosetests-one
                     nosetests-pdb-all nosetests-pdb-module nosetests-pdb-one))
  (autoload nose-func "nosemacs/nose" "" t))
(setq nose-use-verbose nil)

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c t") 'nosetests-all)
            (local-set-key (kbd "C-c m") 'nosetests-module)
            (local-set-key (kbd "C-c .") 'nosetests-one)
            (local-set-key (kbd "C-c p t") 'nosetests-pdb-all)
            (local-set-key (kbd "C-c p m") 'nosetests-pdb-module)
            (local-set-key (kbd "C-c p .") 'nosetests-pdb-one)))

(if (fboundp 'ns-toggle-fullscreen)
    (global-set-key (kbd "M-RET") 'ns-toggle-fullscreen))

(autoload 'iedit-mode "iedit" "" t)
(global-set-key (kbd "C-;") 'iedit-mode)
