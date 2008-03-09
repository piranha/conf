(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-log-done t
      org-return-follows-link t
      org-hide-emphasis-markers t
      org-hide-leading-stars t
      org-odd-levels-only t)

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-,") (fun-for-bind bs--show-with-configuration "org"))))

(global-set-key (kbd "<f1>") (fun-for-bind toggle-file "~/org/life.org"))
(global-set-key (kbd "<f2>") (fun-for-bind toggle-file "~/org/musicx.org"))
(global-set-key (kbd "<f3>") (fun-for-bind toggle-file "~/org/mydeco.org"))
