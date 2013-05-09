(rc/ensure-package-installed 'yasnippet)
(rc/ensure-package-installed 'clojure-snippets)

(defun rc/turn-on-yasnippets ()
  (interactive)
  (yas-minor-mode 1))

(add-hook 'emacs-lisp-mode-hook 'rc/turn-on-yasnippets)
(add-hook 'c++-mode-hook        'rc/turn-on-yasnippets)
(add-hook 'clojure-mode-hook    'rc/turn-on-yasnippets)
