(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((eval progn
           (auto-revert-mode 1)
           (rc/autopull-changes)
           (add-hook
            (quote after-save-hook)
            (quote rc/autocommit-changes)
            nil
            (quote make-it-local)))
     (eval progn
           (rc/autopull-changes)
           (add-hook
            (quote after-save-hook)
            (quote rc/autocommit-changes)
            nil
            (quote make-it-local)))
     (eval when
           (eq system-type
               (quote gnu/linux))
           (rc/autopull-changes)
           (add-hook
            (quote after-save-hook)
            (quote rc/autocommit-changes)
            nil
            (quote make-it-local)))
     (eval when
           (fboundp
            (quote flymake-mode))
           (flymake-mode 1))
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
