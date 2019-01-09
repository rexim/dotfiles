;;; RCs

(package-initialize)                    ;Package.el won't shut up
                                        ;without this line being
                                        ;specificaly here and not
                                        ;anywhere else ~_~
(load "~/.emacs.rc/rc.el")

(load "~/.emacs.rc/misc-rc.el")
(load "~/.emacs.rc/jira-rc.el")
(load "~/.emacs.rc/appearance-rc.el")
(load "~/.emacs.rc/ido-smex-rc.el")
(load "~/.emacs.rc/ccmode-rc.el")
(load "~/.emacs.rc/paredit-rc.el")
(load "~/.emacs.rc/emacs-lisp-rc.el")
(load "~/.emacs.rc/haskell-mode-rc.el")
(load "~/.emacs.rc/whitespace-mode-rc.el")
(load "~/.emacs.rc/magit-rc.el")
(load "~/.emacs.rc/multiple-cursors-rc.el")
(load "~/.emacs.rc/ggtags-rc.el")
(load "~/.emacs.rc/openwith-rc.el")
(load "~/.emacs.rc/dired-rc.el")
(load "~/.emacs.rc/helm-rc.el")
(load "~/.emacs.rc/yasnippet-rc.el")
(load "~/.emacs.rc/rainbow-rc.el")
(load "~/.emacs.rc/word-wrap-rc.el")
(load "~/.emacs.rc/js2-mode-rc.el")
(load "~/.emacs.rc/slime-rc.el")
(load "~/.emacs.rc/nxml-mode-rc.el")
(load "~/.emacs.rc/perl-mode-rc.el")
(load "~/.emacs.rc/tramp-rc.el")
(load "~/.emacs.rc/org-mode-rc.el")
(load "~/.emacs.rc/org-cliplink-rc.el")
(load "~/.emacs.rc/org-babel-rc.el")
(load "~/.emacs.rc/org-capture-rc.el")
(load "~/.emacs.rc/autocommit-rc.el")
(load "~/.emacs.rc/powershell-rc.el")
(load "~/.emacs.rc/elfeed-rc.el")
(load "~/.emacs.rc/jira-markup-mode-rc.el")
(load "~/.emacs.rc/google-translate-rc.el")
(load "~/.emacs.rc/jabber-rc.el")
(load "~/.emacs.rc/erc-rc.el")
(load "~/.emacs.rc/eldoc-mode-rc.el")
(load "~/.emacs.rc/recentf-mode-rc.el")
(load "~/.emacs.rc/neotree-rc.el")
(load "~/.emacs.rc/company-rc.el")
(load "~/.emacs.rc/rtags-rc.el")
(load "~/.emacs.rc/crux-rc.el")
(load "~/.emacs.rc/tide-rc.el")
(load "~/.emacs.rc/merlin-rc.el")
(load "~/.emacs.rc/racer-rc.el")
(load "~/.emacs.rc/flycheck-rc.el")
(load "~/.emacs.rc/elpy-rc.el")
(load "~/.emacs.rc/typescript-rc.el")
(load "~/.emacs.rc/editorconfig-rc.el")
(load "~/.emacs.rc/nasm-mode-rc.el")
(load "~/.emacs.rc/latex-mode-rc.el")
(load "~/.emacs.rc/agda2-mode-rc.el")
(load "~/.emacs.rc/nodejs-repl-rc.el")
(load "~/.emacs.rc/cobol-mode-rc.el")

;;; Packages that don't require configuration
(rc/require
 'scala-mode
 'jade-mode
 'd-mode
 'erlang
 'nsis-mode
 'yaml-mode
 'tt-mode
 'glsl-mode
 'tuareg
 'lua-mode
 'less-css-mode
 'graphviz-dot-mode
 'clojure-mode
 'cmake-mode
 'ack
 'cil-mode
 'groovy-mode
 'rust-mode
 'csharp-mode
 'nim-mode
 'picolisp-mode
 'jinja2-mode
 'htmlize
 'markdown-mode
 'purescript-mode
 'gradle-mode
 'nix-mode
 'dockerfile-mode
 'love-minor-mode
 'toml-mode
 'ensime
 'zeal-at-point
 'elm-mode
 'nginx-mode
 'kotlin-mode
 'org-pomodoro
 'bnfc
 'fixmee
 'buffer-move
 'go-mode
 'php-mode
 'package-lint
 'racket-mode
 'morganey-mode
 'uuidgen
 'handlebars-mode
 'qml-mode
 'ag
 'xquery-mode
 'fsharp-mode
 'emojify
 'hindent
 'sml-mode
 )

(load "~/.emacs.shadow/shadow-rc.el" t)

(setq custom-file "~/.emacs-custom.el")
(load custom-file)
