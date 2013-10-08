(rc/ensure-package-installed 'yasnippet)
(rc/ensure-package-installed 'dropdown-list)

(require 'dropdown-list)
(require 'yasnippet)

;; http://stackoverflow.com/questions/6888189/drop-down-menu-for-emacs
(setq yas/prompt-functions '(yas/dropdown-prompt))
(setq yas-snippet-dirs '("~/.emacs.snippets/"))

(eval-after-load 'yasnippet
  (yas-reload-all))

(defun rc/turn-on-yasnippet ()
  (interactive)
  (yas-minor-mode 1))

(setq rc/yasnippet-compatible-mode-hooks
      '(emacs-lisp-mode-hook
        c-mode-common-hook
        scala-mode-hook
        js-mode-hook
        html-mode-hook
        nxml-mode-hook
        css-mode-hook
        latex-mode-hook))

(dolist (mode-hook rc/yasnippet-compatible-mode-hooks)
  (add-hook mode-hook 'rc/turn-on-yasnippet))
