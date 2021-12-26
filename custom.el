(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-avoid-polling t)
 '(auto-revert-interval 5)
 '(auto-revert-use-notify t)
 '(auto-revert-verbose nil)
 '(blink-cursor-blinks 3)
 '(buffer-auto-revert-by-notification t t)
 '(completion-ignore-case t t)
 '(create-lockfiles nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(global-auto-revert-non-file-buffers t)
 '(ielm-dynamic-return nil)
 '(ielm-header "" t)
 '(ielm-mode-hook '(eldoc-mode))
 '(ielm-prompt "λ> ")
 '(org-src-window-setup 'current-window)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(recentf-max-saved-items nil)
 '(recentf-mode t)
 '(require-final-newline t)
 '(safe-local-variable-values
   '((common-lisp-style . modern)
     (comment-style . multi-line)
     (toc-org-max-depth . 4)
     (eval add-hook 'after-save-hook #'my-compile-init-file 91 t)
     (eval add-hook 'after-save-hook #'org-babel-tangle 90 t)))
 '(scroll-margin 1)
 '(shr-image-animate nil)
 '(use-dialog-box nil)
 '(use-short-answers t)
 '(visible-bell t)
 '(x-select-enable-clipboard-manager nil)
 '(y-or-n-p-use-read-key t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold ((t (:weight normal)))))
