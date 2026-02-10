;; -*- lexical-binding: t; -*-
;;; mccarthy-dark-theme.el -- A dark color theme, inspired by mccarthy-theme.

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

;; Dark companion to mccarthy-theme.  Same hue families, inverted luminance.
;; https://github.com/owainlewis/emacs-color-themes

;;; Code:
(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(deftheme mccarthy-dark "A dark color theme for Emacs.")

(let ((*background*         "#1a1a2e")
      (*comments*           "#cc6660")          ;; warm red, lighter than light-theme's #AA3731
      (*constant*           "#5ec4c4")          ;; teal, lifted from #006969
      (*current-line*       "#2a2a40")          ;; subtle highlight row
      (*cursor-underscore*  "#7eaaff")          ;; soft blue cursor
      (*keywords*           "#7b9ef0")          ;; blue, lifted from #4B6AC8
      (*line-number*        "#2a2a3a")
      (*method-declaration* "#c8cce0")          ;; near-white with a hint of blue
      (*mode-line-bg*       "#282840")
      (*mode-inactive-bg*   "#1e1e30")
      (*mode-line-fg*       "#d0d0e0")
      (*mode-inactive-fg*   "#666680")
      (*normal*             "#c8c8d4")          ;; soft off-white
      (*number*             "#8ab4fc")          ;; from #5b93fc, lighter
      (*operators*          "#9a9ec0")          ;; from #2c3140, inverted
      (*warning*            "#7b9ad8")          ;; from #3b5998, lighter
      (*regexp*             "#f0b0d8")          ;; from #E9C, softened
      (*string*             "#e0a050")          ;; from #BC670F, brighter amber
      (*variable*           "#e86070")          ;; from #D14, lifted
      (*visual-selection*   "#4a4a78")
      (*header-line-bg*     "#0087af"))

  (custom-theme-set-faces
   'mccarthy-dark

   `(bold ((t (:bold t))))
   `(button ((t (:foreground, *keywords* :underline t))))
   `(default ((t (:background, *background* :foreground, *normal*))))
   ;; Colour of header lines in Proced, info, Buffer list etc...
   `(header-line ((t (:background, *header-line-bg* :foreground, *background*))))
   `(highlight ((t (:background, *visual-selection*))))
   `(highlight-face ((t (:background, *visual-selection*))))
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
   `(vertical-border ((t (:foreground, *visual-selection*)))) ;; between splits

   ;; show-paren
   `(show-paren-mismatch ((t (:background, *warning* :foreground, *background* :weight bold))))
   `(show-paren-match ((t (:background, *visual-selection* :foreground, *normal*))))

   ;; which-function
   `(which-func ((t (:foreground, *constant*))))

   ;; whitespace
   `(whitespace-line ((t (:background "#2e2438" :foreground unspecified))))
   `(whitespace-trailing ((t (:background "#3e2030"))))
   `(whitespace-tab ((t (:foreground "#2e2e42"))))
   `(whitespace-space ((t (:foreground "#2e2e42"))))
   `(whitespace-newline ((t (:foreground "#2e2e42"))))

   ;; search
   `(isearch ((t (:background, *header-line-bg* :foreground, *background*))))
   `(isearch-fail ((t (:background, *warning*))))
   `(lazy-highlight ((t (:background, *operators* :foreground, *background*))))

   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mccarthy-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; mccarthy-dark-theme.el ends here
