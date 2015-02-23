;;; RCs
(load "~/.emacs.rc/rc.el")

(load "~/.emacs.rc/misc-rc.el")
(load "~/.emacs.rc/appearance-rc.el")
;; (load "~/.emacs.rc/gotham-theme-rc.el")
(load "~/.emacs.rc/zenburn-theme-rc.el")
;; (load "~/.emacs.rc/gruber-darker-theme-rc.el")
(load "~/.emacs.rc/ido-smex-rc.el")
(load "~/.emacs.rc/ccmode-rc.el")
(load "~/.emacs.rc/paredit-rc.el")
(load "~/.emacs.rc/emacs-lisp-rc.el")
(load "~/.emacs.rc/markdown-mode-rc.el")
(load "~/.emacs.rc/haskell-mode-rc.el")
(load "~/.emacs.rc/magit-rc.el")
(load "~/.emacs.rc/multiple-cursors-rc.el")
(load "~/.emacs.rc/ggtags-rc.el")
(load "~/.emacs.rc/openwith-rc.el")
(load "~/.emacs.rc/dired-rc.el")
(load "~/.emacs.rc/helm-rc.el")
(load "~/.emacs.rc/yasnippet-rc.el")
(load "~/.emacs.rc/rainbow-rc.el")
(load "~/.emacs.rc/js-mode-rc.el")
(load "~/.emacs.rc/slime-rc.el")
(load "~/.emacs.rc/batch-mode-rc.el")
(load "~/.emacs.rc/auto-fill-mode-rc.el")
(load "~/.emacs.rc/nxml-mode-rc.el")
(load "~/.emacs.rc/perl-mode-rc.el")
(load "~/.emacs.rc/fan-mode-rc.el")
(load "~/.emacs.rc/crappy-jsp-mode-rc.el")
(load "~/.emacs.rc/recentf-mode-rc.el")
(load "~/.emacs.rc/tramp-rc.el")
(load "~/.emacs.rc/org-mode-rc.el")
;; (load "~/.emacs.rc/linum-mode-rc.el")
(load "~/.emacs.rc/auto-complete-mode-rc.el")
(load "~/.emacs.rc/powershell-rc.el")

;;; Packages that don't require configuration 
(rc/require 'scala-mode2 'jade-mode 'd-mode
            'erlang 'nsis-mode 'yaml-mode
            'tt-mode 'glsl-mode 'tuareg
            'lua-mode 'less-css-mode 'graphviz-dot-mode
            'clojure-mode 'cmake-mode 'ack
            'cil-mode)

;;; Custom's stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote ((eval progn
                 (rc/autopull-changes)
                 (add-hook (quote after-save-hook)
                           (quote rc/autocommit-changes)
                           nil (quote make-it-local)))
           (eval when (eq system-type (quote gnu/linux))
                 (rc/autopull-changes)
                 (add-hook (quote after-save-hook)
                           (quote rc/autocommit-changes)
                           nil (quote make-it-local)))
           (eval when (fboundp (quote flymake-mode))
                 (flymake-mode 1))
           (eval when (fboundp (quote rainbow-mode))
                 (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
