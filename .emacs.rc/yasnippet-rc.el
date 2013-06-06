(rc/ensure-package-installed 'yasnippet)
(rc/ensure-package-installed 'dropdown-list)

(require 'dropdown-list)
(require 'yasnippet)

;; http://stackoverflow.com/questions/6888189/drop-down-menu-for-emacs
(setq yas/prompt-functions '(yas/dropdown-prompt))

(add-to-list 'yas-snippet-dirs "~/.emacs.snippets/")

(eval-after-load 'yasnippet
  (yas-reload-all))

(defun rc/turn-on-yasnippets ()
  (interactive)
  (yas-minor-mode 1))

(add-hook 'emacs-lisp-mode-hook 'rc/turn-on-yasnippets)
(add-hook 'c-mode-common-hook   'rc/turn-on-yasnippets)
(add-hook 'scala-mode-hook      'rc/turn-on-yasnippets)
(add-hook 'js-mode-hook         'rc/turn-on-yasnippets)
(add-hook 'html-mode-hook       'rc/turn-on-yasnippets)
