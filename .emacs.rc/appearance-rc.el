;;; Fonts
(cond
 ((eq system-type 'windows-nt)
  (add-to-list 'default-frame-alist
               '(font . "Consolas-13")))
 ((eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist
               '(font . "Monospace-15"))))

;;; GUI
(tool-bar-mode 0)
(menu-bar-mode 0)
(column-number-mode 1)
(global-linum-mode 1)
(show-paren-mode 1)

;;; Color theme
(rc/ensure-package-installed 'zenburn-theme)
(load-theme 'zenburn t)

;; FIXME(rexim): for this sort of things a special mode is required.
;; (add-to-list 'custom-theme-load-path
;;              "~/Programming/mine/desert-theme/")
;; (add-to-list 'custom-theme-load-path
;;              "~/Programming/mine/gruber-darker-theme/")
;; (add-to-list 'custom-theme-load-path
;;              "~/Programming/foreign/zenburn-emacs/")

;; (defvar rc/current-theme 'gruber-darker)

;; (defun rc/reload-theme (theme)
;;   (interactive)
;;   (load-theme theme t))

;; (rc/reload-theme rc/current-theme)

;; (global-set-key (kbd "C-c C-b")
;;                 '(lambda ()
;;                    (interactive)
;;                    (rc/reload-theme rc/current-theme)))
