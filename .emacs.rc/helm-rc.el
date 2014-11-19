(rc/require 'helm)
(rc/require 'helm-cmd-t)
(rc/require 'helm-git-grep)

(setq helm-ff-transformer-show-only-basename nil)

(global-set-key (kbd "C-c h t") 'helm-cmd-t)
(global-set-key (kbd "C-c h g") 'helm-git-grep)
(global-set-key (kbd "C-c h f") 'helm-find)
