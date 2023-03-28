;;; alabaster-theme.el -- Alabaster theme for Emacs.

;; Author: Chris Etheridge (theme originally by Nikita Tonsky)
;; URL: https://github.com/chris-etheridge/alabaster-emacs
;; Package-Version: 20160525.0001
;; Version: 1.0

;;; Commentary:
;;
;; Alabaster is a theme originally created by Nikita Tonsky for Light Table.
;; Source: <https://github.com/tonsky/alabaster-lighttable-skin>

(deftheme alabaster
  "Alabster skin.")

(let ((selection-color "#C9D0D9")
      (highlight-color "#EEE00A")
      (secondary-color "#FBE9AD")
      (active-color "#EEEEEE")
      (passive-color "#AAAAAA")
      (subtle-color "#EEEEEE")
      (error-color "#F93232")
      (border-color "#a5a5a5"))
  (custom-theme-set-faces
   'alabaster
   ;; Basics
   '(default ((t (:background "grey99"))))
   '(blue ((t (:foreground "blue"))))
   '(bold ((t (:bold t))))
   '(bold-italic ((t (:italic t :bold t))))
   '(border-glyph ((t (nil))))
   '(green ((t (:foreground "green"))))
   '(info-node ((t (:italic t :bold t))))
   '(info-xref ((t (:bold t))))
   '(italic ((t (:italic t))))
   '(left-margin ((t (nil))))
   '(pointer ((t (nil))))
   '(red ((t (:foreground "red"))))
   '(right-margin ((t (nil))))
   '(underline ((t (:underline t))))
   '(yellow ((t (:foreground "yellow"))))

   ;; Frame
   '(fringe ((t (:background "#f7f7f7"))))
   `(mode-line ((t (:background "#e8e8e8" :foreground "black"
                                :box (:line-width -1 :color ,border-color)))))
   '(mode-line-highlight ((t (:box (:line-width 2 :color "#9599B0")))))
   `(mode-line-inactive
     ((t (:inherit mode-line :background "#f5f5f5" :foreground "grey20"
                   :box (:line-width -1 :color ,border-color) :weight light))))

   ;; Parens
   `(show-paren-match ((t (:background ,passive-color))))
   `(show-paren-mismatch ((t (:foreground "#F9F2CE" :background ,error-color))))

   ;; Highlighting
   `(hl-line ((t (:background ,active-color))))
   `(highline-face ((t (:background ,active-color))))
   `(highlight ((t (:background ,highlight-color))))
   `(highlight-symbol-face ((t (:background ,secondary-color))))
   `(isearch ((t (:foreground "black" :background ,highlight-color))))
   `(lazy-highlight ((t (:background ,secondary-color))))
   `(primary-selection ((t (:background ,selection-color))))
   `(region ((t (:background ,selection-color))))
   `(secondary-selection ((t (:background ,secondary-color))))
   `(shadow ((t (:foreground "grey50" :background ,subtle-color))))
   `(text-cursor ((t (:background "black" :foreground ,passive-color))))
   `(zmacs-region ((t (:background ,selection-color))))

   ;; More
   '(mumamo-background-chunk-submode ((t (:background "#EAEBE6"))))

   ;; Font-lock
   '(font-lock-builtin-face ((t (:foreground "#626FC9"))))
   '(font-lock-comment-face ((t (:foreground "#AA3731"))))
   '(font-lock-constant-face ((t (:foreground "#7653C1" :background "#FFF" ;;"#F3F2FF"
                                              ))))
   '(font-lock-doc-string-face ((t (:foreground "#1A93AE" :background "#F4F9FE"))))
   '(font-lock-function-name-face ((t (:foreground "#4E279A"))))
   '(font-lock-keyword-face ((t (:foreground "#6700B9"))))
   '(font-lock-preprocessor-face ((t (:foreground "#434343"))))
   '(font-lock-reference-face ((t (:foreground "#4E279A" :background "#F3F2FF"))))
   '(font-lock-string-face ((t (:foreground "#BC670F" :background "#FFF" ;;"#FDFBF5"
                                            ))))
   '(font-lock-type-face ((t (:foreground "#547e2b"))))
   '(font-lock-variable-name-face ((t (:foreground "#576336"))))
   '(font-lock-warning-face ((t (:foreground "#F93232"))))

   ;; Diff Mode
   '(diff-file-header ((t (:bold t :inherit diff-header))))
   '(diff-header ((t (:background "#DDDDFF" :foreground "grey20"))))
   '(diff-added ((t (:background "#DDFFDD"))))
   '(diff-removed ((t (:background "#FFDDDD"))))
   '(diff-changed ((t (:background "#FFFFDD"))))
   '(diff-refine-change ((t (:background "#DDDDFF"))))

   ;; Magit
   '(magit-diff-file-header ((t (:bold t :inherit diff-header))))
   '(magit-diff-hunk-header ((t (:inherit diff-header))))
   '(magit-diff-add ((t (:inherit diff-added :foreground "grey20"))))
   '(magit-diff-del ((t (:inherit diff-removed :foreground "grey20"))))
   '(magit-diff-none ((t (:inherit diff-context :foreground "grey20"))))
   '(magit-item-highlight ((t (:background nil :foreground "black"))))

   ;; Done
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'alabaster)

;;; alabaster-theme.el ends here
