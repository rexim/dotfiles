;;; RCs
(load "~/.emacs.rc/rc.el")

(load "~/.emacs.rc/misc-rc.el")
(load "~/.emacs.rc/jira-rc.el")
(load "~/.emacs.rc/appearance-rc.el")
(load "~/.emacs.rc/gruber-darker-theme-rc.el")
(load "~/.emacs.rc/ido-smex-rc.el")
(load "~/.emacs.rc/ccmode-rc.el")
(load "~/.emacs.rc/paredit-rc.el")
(load "~/.emacs.rc/emacs-lisp-rc.el")
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
;; (load "~/.emacs.rc/auto-fill-mode-rc.el")
(load "~/.emacs.rc/nxml-mode-rc.el")
(load "~/.emacs.rc/perl-mode-rc.el")
(load "~/.emacs.rc/tramp-rc.el")
(load "~/.emacs.rc/org-mode-rc.el")
(load "~/.emacs.rc/org-cliplink-rc.el")
(load "~/.emacs.rc/org-capture-rc.el")
;; (load "~/.emacs.rc/linum-mode-rc.el")
;; (load "~/.emacs.rc/auto-complete-mode-rc.el")
(load "~/.emacs.rc/powershell-rc.el")
(load "~/.emacs.rc/elfeed-rc.el")
(load "~/.emacs.rc/jira-markup-mode-rc.el")
(load "~/.emacs.rc/google-translate-rc.el")
(load "~/.emacs.rc/jabber-rc.el")
(load "~/.emacs.rc/erc-rc.el")
(load "~/.emacs.rc/eldoc-mode-rc.el")
(load "~/.emacs.rc/recentf-mode-rc.el")
(load "~/.emacs.rc/neotree-rc.el")
(load "~/.emacs.rc/flycheck-rc.el")
(load "~/.emacs.rc/company-rc.el")
(load "~/.emacs.rc/rtags-rc.el")
(load "~/.emacs.rc/cmake-ide-rc.el")

;;; Packages that don't require configuration 
(rc/require 'scala-mode2 'jade-mode 'd-mode
            'erlang 'nsis-mode 'yaml-mode
            'tt-mode 'glsl-mode 'tuareg
            'lua-mode 'less-css-mode 'graphviz-dot-mode
            'clojure-mode 'cmake-mode 'ack
            'cil-mode 'groovy-mode 'rust-mode
            'csharp-mode 'nim-mode 'picolisp-mode
            'jinja2-mode 'htmlize 'markdown-mode
            'purescript-mode)

(setq custom-file "~/.emacs-custom.el")
(load custom-file)
