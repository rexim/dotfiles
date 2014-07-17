(rc/ensure-package-installed 'yasnippet)
(rc/ensure-package-installed 'dropdown-list)

(require 'dropdown-list)
(require 'yasnippet)

(setq yas/triggers-in-field nil)

;; http://stackoverflow.com/questions/6888189/drop-down-menu-for-emacs
(setq yas/prompt-functions '(yas/dropdown-prompt))
(setq yas-snippet-dirs '("~/.emacs.snippets/"))

(yas-global-mode 1)
