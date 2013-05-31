;;; Fonts
(cond
 ((eq system-type 'windows-nt) (set-default-font "Consolas-13"))
 ((eq system-type 'gnu/linux)  (set-default-font "Monospace-15")))

;;; GUI
(tool-bar-mode 0)
(menu-bar-mode 0)
(column-number-mode 1)
(global-linum-mode 1)
(show-paren-mode 1)

;;; Color theme
;; (rc/ensure-package-installed 'gruber-darker-theme)
;; (load-theme 'gruber-darker t)

(rc/ensure-package-installed 'zenburn-theme)
(load-theme 'zenburn t)
