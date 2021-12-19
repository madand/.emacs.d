;;; test-theme.el --- Theme Testing Helper        -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Andriy Kmit'

;; Author: Andriy Kmit' <dev@madand.net>
;; URL: https://github.com/madand/.emacs.d

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Helper for trying out themes.

;; The intent is to be able load a theme and be sure its looks aren't affected
;; by other themes and/or packages.
;;
;; Usage:
;;   emacs -Q -l test-theme.el -wombat
;; where "wombat" is a theme name; you may use any of the available ones.
;;
;; To see a list of the available themes use the following command:
;;   emacs -Q --batch --eval '(message "%s" (custom-available-themes))'

;;; Code:



(defconst my-sample-file (expand-file-name "init.el" user-emacs-directory))
(defconst my-sample-url (format "file://%s"
                                (expand-file-name "~/docs/CommonLisp/gray-streams.html")))
;; (defconst my-sample-url "https://www.nhplace.com/kent/CL/Issues/stream-definition-by-user.html")



;;; Fonts

(defvar my-default-font (font-spec :family "Iosevka SS09" :size 16))
;; (defvar my-mono-serif-font (font-spec :name "Iosevka Fixed Slab"))
(defvar my-proportional-font (font-spec :family "Roboto" :size 18))

(set-face-font 'default my-default-font)
(set-face-font 'fixed-pitch my-default-font)
(set-face-font 'fixed-pitch-serif my-default-font)
(set-face-font 'variable-pitch my-proportional-font)
;; Websites in `eww' look much better with this:
(set-face-attribute 'variable-pitch nil :slant 'unspecified :weight 'unspecified)

(set-frame-font my-default-font t t)

(setq text-scale-mode-step 1.1)



(defun my-theme-option-handler (switch)
  "Handle command line switch by loading a theme with that name."
  (let ((theme-name (substring switch 1)))
    (message "Loading theme: %s" theme-name)
    (load-theme (intern theme-name) t)))

(defun my-register-theme-switches ()
  "Register command line switches for all built-in themes.
So you can enable the theme by starting Emacs with -<theme-name>,
for example:
emacs -Q -l test-theme.el -wombat"
  (let (handlers)
    (dolist (theme (custom-available-themes) handlers)
      (add-to-list 'command-switch-alist (cons (format "-%s" theme)
                                               #'my-theme-option-handler)))))

(my-register-theme-switches)



(defun my-open-windows ()
  "Disable GUI frills, then open some buffers to see the theme in action."
  ;; Disable GUI bells and whistles.
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (tooltip-mode -1)
  ;; Now open stuff to see the theme in action.
  (split-window-right)
  (split-window-right)
  (balance-windows)
  (info "(emacs) Minibuffer History")
  (other-window 1)
  (find-file my-sample-file)
  (other-window 1)
  (redisplay t)
  (eww my-sample-url ))

(add-hook 'emacs-startup-hook #'my-open-windows)

;;; test-theme.el ends here
