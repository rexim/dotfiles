(rc/ensure-package-installed 'helm)
(rc/ensure-package-installed 'helm-cmd-t)
(rc/ensure-package-installed 'helm-git-grep)

(global-set-key (kbd "C-c h t") 'helm-cmd-t)
(global-set-key (kbd "C-c h g") 'helm-git-grep)
