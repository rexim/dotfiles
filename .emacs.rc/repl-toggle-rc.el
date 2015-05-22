(rc/require 'repl-toggle)

(setq rtog/fullscreen t)
(require 'repl-toggle)
(setq rtog/mode-repl-alist '((emacs-lisp-mode . ielm)
                             (js-mode . nodejs-repl)))
