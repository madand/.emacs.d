;;; early-init.el --- The Early Init File            -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Andriy Kmit'

;; Author: Andriy Kmit' <dev@madand.net>
;; URL: https://github.com/madand/emacs.d

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation version 3 of the License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; More info:
;; * (info "(emacs) Early Init File")
;; * https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html

;;; Code:


;;; Some tricks to speed the startup up

;; 1 MiB between each garbage collection.
(defconst my-gc-cons-threshold (* 1 (expt 1024 2))
  "Value of `gc-cons-threshold' to be used after initialization.")
;; Effectively disable the garbage collector during the initialization.
(defconst my-init-gc-cons-threshold (expt 1024 3)
  "Value of `gc-cons-threshold' during Emacs initialization.")
(defconst my-init-gc-cons-percentage 0.8
  "Value of `gc-cons-percentage' during Emacs initialization.")

(let ((original-gc-cons-percentage gc-cons-percentage)
      (original-file-name-handler-alist file-name-handler-alist))
  (setq gc-cons-threshold my-init-gc-cons-threshold)
  (setq gc-cons-percentage my-init-gc-cons-percentage)
  (setq file-name-handler-alist nil)
  
  (add-hook 'after-init-hook
            (defun my-set-settings-after-startup ()
              "Restore settings overridden for faster startup (GC and stuff)."
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds" (float-time
                                               (time-since before-init-time)))
                       gcs-done)
              (setq gc-cons-threshold my-gc-cons-threshold)
              (setq gc-cons-percentage original-gc-cons-percentage)
              (setq file-name-handler-alist original-file-name-handler-alist))))


;;; Early customization

;; Disable package.el
(setq package-enable-at-startup nil)
;; Always load newest lisp files, even if stale *.elc exists nearby.
(setq load-prefer-newer t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . nil) default-frame-alist)
(push '(tool-bar-lines . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;;; early-init.el ends here
