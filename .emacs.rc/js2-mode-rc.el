(rc/require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node". js2-jsx-mode))
