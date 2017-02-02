(rc/require 'rainbow-mode)

(defun rc/turn-on-rainbow-mode ()
  (interactive)
  (rainbow-mode 1))

(add-hook 'html-mode-hook 'rc/turn-on-rainbow-mode)
(add-hook 'css-mode-hook  'rc/turn-on-rainbow-mode)
(add-hook 'js-mode-hook   'rc/turn-on-rainbow-mode)
(add-hook 'nxml-mode-hook 'rc/turn-on-rainbow-mode)
(add-hook 'conf-xdefaults-mode-hook 'rc/turn-on-rainbow-mode)
(add-hook 'typescript-mode-hook 'rc/turn-on-rainbow-mode)
