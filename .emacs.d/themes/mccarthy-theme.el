;; -*- lexical-binding: t; -*-
;;; mccarthy-theme.el -- A light color theme.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; https://github.com/owainlewis/emacs-color-themes

;;; Code:
(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(deftheme mccarthy "A light color theme for Emacs.")

(let ((*background*         "#fff")
      (*comments*           "#AA3731") ;; "#2c5115"
      (*constant*           "#006969")
      (*current-line*       "#EEE00A") ;; "#1b1e2b"
      (*cursor-underscore*  "#0e1f5b")
      (*keywords*           "#4B6AC8")
      (*line-number*        "#EEE")
      (*method-declaration* "#1b1e2b")
      (*mode-line-bg*       "#e8e8e8")
      (*mode-inactive-bg*   "#f5f5f5")
      (*mode-line-fg*       "black")
      (*mode-inactive-fg*   "grey30")
      (*normal*             "black")
      (*number*             "#5b93fc")
      (*operators*          "#2c3140")
      (*warning*            "#3b5998")
      (*regexp*             "#E9C")
      (*string*             "#BC670F") ;;"#555"
      (*variable*           "#D14")
      (*visual-selection*   "#CCC")
      (*header-line-bg*     "#0087af"))

  (custom-theme-set-faces
   'mccarthy

   `(bold ((t (:bold t))))
   `(button ((t (:foreground, *keywords* :underline t))))
   `(default ((t (:background, *background* :foreground, *normal*))))
   ;; Colour of header lines in Proced, info, Buffer list etc...
   `(header-line ((t (:background, *header-line-bg* :foreground, *background*))))
   `(highlight ((t (:background, *current-line*))))
   `(highlight-face ((t (:background, *current-line*))))
   `(hl-line ((t (:background, *current-line* :underline t))))
   `(info-xref ((t (:foreground, *keywords* :underline t))))
   `(region ((t (:background, *visual-selection*))))
   `(underline ((nil (:underline t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground, *operators*))))
   `(font-lock-comment-delimiter-face ((t (:foreground, *comments*))))
   `(font-lock-comment-face ((t (:foreground, *comments*))))
   `(font-lock-constant-face ((t (:foreground, *constant*))))
   `(font-lock-doc-face ((t (:foreground, *string*))))
   `(font-lock-doc-string-face ((t (:foreground, *string*))))
   `(font-lock-function-name-face ((t (:foreground, *method-declaration*))))
   `(font-lock-keyword-face ((t (:foreground, *keywords*))))
   `(font-lock-negation-char-face ((t (:foreground, *warning*))))
   `(font-lock-number-face ((t (:foreground, *number*))))
   `(font-lock-preprocessor-face ((t (:foreground, *keywords*))))
   `(font-lock-reference-face ((t (:foreground, *constant*))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground, *regexp*))))
   `(font-lock-regexp-grouping-construct ((t (:foreground, *regexp*))))
   `(font-lock-string-face ((t (:foreground, *string*))))
   `(font-lock-type-face ((t (:foreground, *operators*))))
   `(font-lock-variable-name-face ((t (:foreground, *variable*))))
   `(font-lock-warning-face ((t (:foreground, *warning*))))

   ;; GUI
   `(fringe ((t (:background, *mode-inactive-bg*))))
   `(linum ((t (:background, *line-number*))))
   `(minibuffer-prompt ((t (:foreground, *constant*))))
   `(mode-line ((t (:background, *mode-line-bg* :foreground, *mode-line-fg*))))
   `(mode-line-inactive ((t (:background, *mode-inactive-bg* :foreground, *mode-inactive-fg*))))
   `(cursor ((t (:background, *cursor-underscore*))))
   `(text-cursor ((t (:background, *cursor-underscore*))))
   `(vertical-border ((t (:foreground, *background*)))) ;; between splits

   ;; show-paren
   `(show-paren-mismatch ((t (:background, *warning* :foreground, *normal* :weight bold))))
   `(show-paren-match ((t (:background, *current-line* :foreground, *normal*))))

   ;; whitespace
   `(whitespace-line ((t (:background "#fff3e0" :foreground unspecified))))
   `(whitespace-trailing ((t (:background "#fdd"))))
   `(whitespace-tab ((t (:foreground "#ddd"))))
   `(whitespace-space ((t (:foreground "#ddd"))))
   `(whitespace-newline ((t (:foreground "#ddd"))))

   ;; search
   `(isearch ((t (:background, *header-line-bg* :foreground, *background*))))
   `(isearch-fail ((t (:background, *warning*))))
   `(lazy-highlight ((t (:background, *operators* :foreground, *visual-selection*))))

   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mccarthy)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; mccarthy-theme.el ends here
