;; (rc/ensure-package-installed 'gruber-darker-theme)

(add-to-list 'custom-theme-load-path
             "~/Programming/mine/gruber-darker-theme/")

(load-theme 'gruber-darker t)

(global-set-key (kbd "C-x t r")
                '(lambda ()
                   (interactive)
                   (load-theme 'gruber-darker t)))
