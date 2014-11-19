;;; Fonts
(cond
 ((eq system-type 'windows-nt)
  (add-to-list 'default-frame-alist
               '(font . "Consolas-13")))
 ((eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist
               '(font . "Ubuntu Mono-14"))))

;;; GUI
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
