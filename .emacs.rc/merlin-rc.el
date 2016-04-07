(setq opam-share (substring
                  (shell-command-to-string "opam config var share 2> /dev/null")
                  0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(require 'merlin)

(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)

(setq merlin-use-auto-complete-mode 'easy)

(setq merlin-command 'opam)
