(defun dwim-helm-find-files-up-one-level-maybe ()
  (interactive)
  (if (looking-back "/" 1)
      (call-interactively 'helm-find-files-up-one-level)
    (delete-char -1)))

(defun dwim-helm-find-files-navigate-forward (orig-fun &rest args)
  "Adjust how helm-execute-persistent actions behaves, depending on context"
  (if (file-directory-p (helm-get-selection))
      (apply orig-fun args)
    (helm-maybe-exit-minibuffer)))

(el-get-bundle helm
  (helm-mode 0)
  (require 'helm-config)

  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-.") 'helm-mini)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-Y") 'helm-show-kill-ring)
  (global-set-key (kbd "M-A") 'helm-semantic-or-imenu)
  (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
  (global-set-key (kbd "M-p") 'helm-for-files)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)

  (define-key helm-buffer-map (kbd "C-k")
    (lambda ()
      (interactive)
      (with-helm-alive-p
        (setq helm-saved-action 'helm-kill-marked-buffers)
        (helm-execute-selection-action))))

  (define-key helm-read-file-map (kbd "<backsqpace>") 'dwim-helm-find-files-up-one-level-maybe)
  (define-key helm-read-file-map (kbd "DEL") 'dwim-helm-find-files-up-one-level-maybe)
  (define-key helm-find-files-map (kbd "<backspace>") 'dwim-helm-find-files-up-one-level-maybe)
  (define-key helm-find-files-map (kbd "DEL") 'dwim-helm-find-files-up-one-level-maybe)

  (define-key helm-map (kbd "<return>") 'helm-maybe-exit-minibuffer)
  (define-key helm-map (kbd "RET") 'helm-maybe-exit-minibuffer)
  (define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "<return>") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "RET") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "RET") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "/") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "/") 'helm-execute-persistent-action)

  (advice-add 'helm-execute-persistent-action :around #'dwim-helm-find-files-navigate-forward)

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  (setq helm-split-window-in-side-p           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-scroll-amount                    8
        helm-ff-file-name-history-use-recentf t
        helm-M-x-fuzzy-match                  t
        helm-buffers-fuzzy-matching           t
        helm-recentf-fuzzy-match              t
        helm-imenu-fuzzy-match                t
        helm-semantic-fuzzy-match             t
        helm-echo-input-in-header-line        t
        helm-quick-update                     t
        helm-locate-fuzzy-match               nil
        helm-locate-command                   "locate-with-mdfind %.0s %s"))

(el-get-bundle helm-ag)
(el-get-bundle helm-swoop
  (global-set-key (kbd "C-:") 'helm-swoop))

(add-hook 'eshell-mode-hook
          '(lambda ()
             (eshell-cmpl-initialize)
             (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)
             (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

;; ido-like helm

(el-get-bundle flx)
