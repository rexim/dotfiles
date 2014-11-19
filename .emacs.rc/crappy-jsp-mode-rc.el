;;; Crappy mode for crappy technology.
(rc/require 'crappy-jsp-mode)
(require 'crappy-jsp-mode)
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . crappy-jsp-mode))
