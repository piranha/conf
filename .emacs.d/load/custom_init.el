(put 'test-case-name 'safe-local-variable '(lambda (x) t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0261c7e47f570afd984db039e083ec0f89eb4194" default)))
 '(js2-basic-offset 4)
 '(js2-mode-indent-ignore-first-tab t)
 '(org-agenda-files (quote ("~/org/life.org")))
 '(safe-local-variable-values
   (quote
    ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t)
     (js-indent-level . 2)
     (encoding . utf-8)
     (prompt-to-byte-compile))))
 '(semantic-default-submodes
   (quote
    (global-semantic-idle-scheduler-mode global-semanticdb-minor-mode)))
 '(vc-handled-backends nil)
 '(wakatime-api-key "70811b9b-9fd6-4863-9ec3-f1ac5d8746c5")
 '(wakatime-cli-path "/Users/piranha/dev/misc/wakatime/wakatime-cli.py"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:foreground "dark green"))))
 '(org-column ((t (:strike-through nil :underline nil :weight normal :height 120 :family "outline-unifont"))))
 '(org-hide ((t (:foreground "grey77")))))
