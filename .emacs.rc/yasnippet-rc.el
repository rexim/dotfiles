(rc/ensure-package-installed 'yasnippet)

(defun rc/turn-on-yasnippets ()
  (interactive)
  (yas-minor-mode 1))

(add-hook 'emacs-lisp-mode-hook 'rc/turn-on-yasnippets)
(add-hook 'c++-mode-hook        'rc/turn-on-yasnippets)
