(global-unset-key "\C-x\C-c") ;; too easy to hit accidentally
(global-set-key "\C-x\C-c\C-v" 'save-buffers-kill-terminal)

(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-,") 'bs-show)
(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-x C-r") 'query-replace-regexp)

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
(global-set-key (kbd "C-a") 'beginning-of-line-dwim)
(global-set-key (kbd "<home>") 'beginning-of-line-dwim)

(global-set-key (kbd "M-v") 'yank) ;; jumpcut support

(global-set-key (kbd "C-M--") 'flycheck-previous-error)
(global-set-key (kbd "C-M-=") 'flycheck-next-error)

;; windows
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-o") 'other-window)
(define-key dired-mode-map (kbd "M-o") 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)

(if (fboundp 'w32-send-sys-command)
    (global-set-key (kbd "M-M") ; maximize
                    (fun-for-bind w32-send-sys-command #xf030 nil)))

(el-get-bundle iedit
  (global-set-key (kbd "C-;") 'iedit-mode))

(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-unset-key (kbd "C-/")) ;; 'yas-expand

(defvar sgml-mode-map)
(with-eval-after-load 'sgml-mode
  (define-key sgml-mode-map (kbd "C-c C-<left>") 'sgml-skip-tag-backward)
  (define-key sgml-mode-map (kbd "C-c C-<right>") 'sgml-skip-tag-forward))

(global-set-key (kbd "M-t")
                (lambda ()
                  (interactive)
                  (start-process
                   "switch-to-chrome" nil
                   "osascript" "-e" "tell application \"Google Chrome\" to activate")))

(global-set-key (kbd "M-A") 'imenu)

 ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
(define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward) ; single key, useful
(define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward) ; single key, useful

(define-key comint-mode-map (kbd "C-M-l") nil)
