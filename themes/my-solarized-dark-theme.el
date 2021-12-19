;;; my-solarized-dark-theme.el --- Customized variant of the Solarized Dark Theme  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Andriy Kmit'

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

;; Personal customization of the Solarized Dark theme.

;;; Code:

(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

(defun my-theme-test ()
  (message "tested"))

(eval-and-compile
  (defvar my-solarized-faces
    '("My personal solarized theme customization."
      (custom-theme-set-faces
       theme-name
       `(link ((,class (:foreground ,blue :underline t :weight bold))))
       `(link-visited ((,class (:foreground ,violet :underline t :weight bold))))
       `(button ((,class (:foreground ,blue :underline t))))
       `(which-key-command-description-face ((,class (:foreground ,blue))))
       `(frog-menu-posframe-background-face ((,class (:background ,base02))))
       ;; info
       `(Info-quoted ((,class (:foreground ,cyan :inherit fixed-pitch-serif))))
       ;; tree-sitter-hl-mode
       `(tree-sitter-hl-face:number ((,class (:foreground ,red :inherit tree-sitter-hl-face:constant))))
       `(tree-sitter-hl-face:type.builtin ((,class (:foreground ,yellow-d :inherit font-lock-builtin-face))))
       `(which-key-key-face ((,class (:foreground ,yellow :bold t)))))
      )))

(deftheme my-solarized-dark "Customized variant of the Solarized Dark color theme")

(solarized-with-color-variables 'dark 'my-solarized-dark
  solarized-dark-color-palette-alist my-solarized-faces)

(provide-theme 'my-solarized-dark)

(provide 'my-solarized-dark-theme)

;;; my-solarized-dark-theme.el ends here
