
(let* ((opam-executable (executable-find "opam"))
       (opam-config-command (when opam-executable
                              (concat opam-executable
                                      " config var share 2> /dev/null")))
       (opam-share (when opam-config-command
                     (substring (shell-command-to-string opam-config-command)
                                0 -1))))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
  (when (require 'merlin)

    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)

    (setq merlin-use-auto-complete-mode 'easy)

    (setq merlin-command 'opam)))
