(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-blinks 3)
 '(org-src-window-setup 'current-window)
 '(safe-local-variable-values
   '((common-lisp-style . modern)
     (comment-style . multi-line)
     (toc-org-max-depth . 4)
     (eval add-hook 'after-save-hook #'my-compile-init-file 91 t)
     (eval add-hook 'after-save-hook #'org-babel-tangle 90 t))))
