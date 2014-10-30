;;; RCs
(load "~/.emacs.rc/package-manager-rc.el")
(load "~/.emacs.rc/misc-rc.el")
(load "~/.emacs.rc/cliplink-rc.el")
(load "~/.emacs.rc/appearance-rc.el")
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
(load "~/.emacs.rc/dirtree-rc.el")
(rc/ensure-package-installed 'scala-mode2)
(rc/ensure-package-installed 'jade-mode)
(rc/ensure-package-installed 'd-mode)
(rc/ensure-package-installed 'erlang)
(rc/ensure-package-installed 'nsis-mode)
(rc/ensure-package-installed 'yaml-mode)
(rc/ensure-package-installed 'tt-mode)
(rc/ensure-package-installed 'glsl-mode)
(rc/ensure-package-installed 'tuareg)
(rc/ensure-package-installed 'lua-mode)
(rc/ensure-package-installed 'less-css-mode)
(rc/ensure-package-installed 'graphviz-dot-mode)
(rc/ensure-package-installed 'clojure-mode)

;;; Custom's stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Documents/Cisco/Cisco.org"
                            "~/Documents/Personal/Tasks.org"
                            "~/Documents/Personal/Imported.org"
                            "~/Documents/Personal/Archive.org")))
 '(safe-local-variable-values
   (quote ((eval when (fboundp (quote flymake-mode)) (flymake-mode 1))
           (eval when (fboundp (quote rainbow-mode)) (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
