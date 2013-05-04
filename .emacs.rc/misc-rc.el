(rc/ensure-package-installed 'rainbow-mode)

(global-set-key (kbd "C-c <C-left>")  'windmove-left)
(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-up>")    'windmove-up)
(global-set-key (kbd "C-c <C-down>")  'windmove-down)

(setq-default inhibit-splash-screen t
              make-backup-files nil
              tab-width 4
              indent-tabs-mode nil)

(cond
 ((eq system-type 'windows-nt) (set-default-font "Consolas-13"))
 ((eq system-type 'gnu/linux)  (set-default-font "Monospace-12")))

(tool-bar-mode 0)
(menu-bar-mode 0)
(column-number-mode 1)
(global-linum-mode 1)
(show-paren-mode 1)
