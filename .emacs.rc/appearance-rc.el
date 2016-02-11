;;; Fonts
(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-13")
   ((eq system-type 'gnu/linux) (if (getenv "REXIM_STREAMER")
                                    "Ubuntu Mono-18"
                                  "Ubuntu Mono-14"))))

(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))

;;; GUI
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

;;; Themes
(rc/require-theme 'zenburn)
