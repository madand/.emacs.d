;;; playground.el --- The Emacs Lisp Playground        -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Andriy Kmit'

;; Author: Andriy Kmit' <dev@madand.net>
;; URL: https://github.com/madand/emacs.d

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

;; Some random Elisp I fiddle with.

;;; Code:


;;; Font size testing utils, adjusting the :height attribute

(defun adjust-face-height (face increment)
  (set-face-attribute face (window-frame)
		      :height (+ increment (face-attribute face :height))))

(defun face-height-increase (face)
  (lambda ()
    (interactive)
    (adjust-face-height face 5)
    (message "%s" (face-attribute face :height))))

(defun face-height-decrease (face)
  (lambda ()
    (interactive)
    (adjust-face-height face -5)
    (message "%s" (face-attribute face :height))))

(global-set-key (kbd "<f1>") (face-height-decrease 'default))
(global-set-key (kbd "<f2>") (face-height-increase 'default))


;;; Font size testing utils, adjusting the :size attribute

(defvar my-fonted-faces '(default fixed-pitch fixed-pitch-serif variable-pitch))

(defvar my-face-size-alist (cl-loop for face in my-fonted-faces
				    collect (cons face 18)))

(defun my-adjust-face-size (face increment)
  (let* ((old-size (alist-get face my-face-size-alist 18))
	 (new-size (setf (alist-get face my-face-size-alist)
			 (+ old-size increment))) )
    (set-face-font face (font-spec :size new-size) (window-frame))
    (message "%s: %s" face new-size)))

(defvar my--adjust-face-size-repeat-face nil)

(defvar my-adjust-face-size-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f1>") #'my-adjust-face-size-decrease)
    (define-key map (kbd "<f2>") #'my-adjust-face-size-increase)
    map))

(defun my-adjust-face-size-command (arg increment)
  (when (or (null my--adjust-face-size-repeat-face)
	    arg)
    (setq my--adjust-face-size-repeat-face
	  (intern (completing-read "Face: " my-fonted-faces nil t))))
  (my-adjust-face-size my--adjust-face-size-repeat-face increment))

(defun my-adjust-face-size-increase (arg)
  (interactive "P")
  (my-adjust-face-size-command arg +1))
(put 'my-adjust-face-size-increase 'repeat-map 'my-adjust-face-size-repeat-map)

(defun my-adjust-face-size-decrease (arg)
  (interactive "P")
  (my-adjust-face-size-command arg -1))
(put 'my-adjust-face-size-decrease 'repeat-map 'my-adjust-face-size-repeat-map)

(repeat-mode)

(global-set-key (kbd "<f1>") #'my-adjust-face-size-decrease)
(global-set-key (kbd "<f2>") #'my-adjust-face-size-increase)



;;; playground.el ends here
