(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-hscroll-mode 'current-line)
 '(auto-revert-avoid-polling t)
 '(auto-revert-interval 5)
 '(auto-revert-use-notify t)
 '(auto-revert-verbose nil)
 '(backup-by-copying t)
 '(blink-cursor-mode nil)
 '(browse-url-browser-function 'eww-browse-url)
 '(browse-url-secondary-browser-function 'browse-url-firefox)
 '(buffer-auto-revert-by-notification t t)
 '(calendar-week-start-day 1)
 '(completion-ignore-case t t)
 '(create-lockfiles nil)
 '(dired-auto-revert-buffer t)
 '(dired-do-revert-buffer t)
 '(dired-dwim-target 'dired-dwim-target-recent)
 '(dired-guess-shell-alist-user
   '(("\\.\\(pdf\\)\\'" "zathura")
     ("\\.\\(mkv\\|mp4\\|mpg\\|vob\\|avi\\)\\'" "mpv")))
 '(dired-isearch-filenames 'dwim)
 '(dired-listing-switches "-alh")
 '(dired-mode-hook '(dired-extra-startup dired-hide-details-mode))
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eshell-history-size 1000)
 '(global-auto-revert-non-file-buffers t)
 '(ielm-dynamic-return nil)
 '(ielm-header "" t)
 '(ielm-mode-hook '(eldoc-mode))
 '(ielm-prompt "λ> ")
 '(indent-tabs-mode nil)
 '(isearch-lazy-count t)
 '(kill-do-not-save-duplicates t)
 '(magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside magit-diff-highlight-hunk-region-using-overlays))
 '(orderless-component-separator " +\\|[-/]")
 '(orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism))
 '(org-hide-emphasis-markers t)
 '(org-roam-completion-everywhere t)
 '(org-src-window-setup 'current-window)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(recenter-positions '(top middle bottom))
 '(recentf-max-saved-items nil)
 '(require-final-newline t)
 '(safe-local-variable-values
   '((common-lisp-style . modern)
     (comment-style . multi-line)
     (toc-org-max-depth . 4)
     (eval add-hook 'after-save-hook #'my-compile-init-file 91 t)
     (eval add-hook 'after-save-hook #'org-babel-tangle 90 t)))
 '(scroll-margin 1)
 '(shr-bullet "• ")
 '(shr-image-animate nil)
 '(shr-max-width nil)
 '(use-dialog-box nil)
 '(use-short-answers t)
 '(view-read-only t)
 '(visible-bell t)
 '(wgrep-auto-save-buffer t)
 '(which-key-idle-delay 0.8)
 '(which-key-idle-secondary-delay 0.3)
 '(which-key-max-description-length nil)
 '(which-key-side-window-location '(right bottom))
 '(which-key-side-window-max-width 0.4)
 '(x-select-enable-clipboard-manager nil)
 '(y-or-n-p-use-read-key t t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold ((t (:weight extra-bold))))
 '(variable-pitch ((t (:height 196 :width normal :foundry "GOOG" :family "Noto Serif")))))
