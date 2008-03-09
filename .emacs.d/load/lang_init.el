(setq default-input-method "russian-computer")

(when nix
  (set-selection-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq
   x-select-enable-clipboard t
   x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
   interprogram-paste-function (quote x-cut-buffer-or-selection-value)
   ))

;; I like having unix lineendings even on windows
(prefer-coding-system 'utf-8-unix)
