(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-autojoin-channels-alist (quote (("irc.freenode.net" "#haskell" "#emacs"))))
 '(jabber-account-list (quote (("rexim@jabber.ru") ("reximorning@jabber.ru"))))
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-exporter-settings (quote ((org-agenda-tag-filter-preset (list "+personal")))))
 '(org-cliplink-transport-implementation (quote url-el))
 '(org-enforce-todo-dependencies nil)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-refile-use-outline-path (quote file))
 '(package-selected-packages
   (quote
    (zeal-at-point yaml-mode xquery-mode uuidgen tuareg tt-mode toml-mode tide smex slime rtags rainbow-mode racket-mode racer qml-mode purescript-mode powershell picolisp-mode php-mode paredit pacmacs package-lint org-pomodoro org-cliplink openwith nsis-mode nodejs-repl nix-mode nim-mode nginx-mode neotree nasm-mode multiple-cursors morganey-mode markdown-mode magit love-minor-mode less-css-mode kotlin-mode js2-mode jira-markup-mode jinja2-mode jade-mode jabber ido-ubiquitous htmlize helm-ls-git helm-git-grep helm-cmd-t haskell-mode handlebars-mode gruber-darker-theme groovy-mode graphviz-dot-mode gradle-mode google-translate go-mode glsl-mode ggtags fsharp-mode flycheck-rust fixmee evil erlang ensime emojify elpy elm-mode elfeed editorconfig dropdown-list dockerfile-mode d-mode csv-nav csharp-mode crux cmake-mode clojure-mode cil-mode buffer-move bnfc batch-mode ag ack)))
 '(safe-local-variable-values
   (quote
    ((haskell-process-args-cabal-new-repl "--ghc-option=-ferror-spans" "--ghc-option=-Wno-missing-home-modules" "exe:HyperNerd")
     (haskell-process-args-cabal-repl "--ghc-option=-ferror-spans" "--ghc-option=-Wno-missing-home-modules" "exe:poutro")
     (haskell-process-args-cabal-repl "--ghc-option=-ferror-spans" "--ghc-option=-Wno-missing-home-modules" "exe:HyperNerd")
     (fixmee-mode . t)
     (eval progn
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
     (eval rc/autocommit-dir-locals)
     (eval when
           (fboundp
            (quote flymake-mode))
           (flymake-mode 1))
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1)))))
 '(whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
