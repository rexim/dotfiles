(defun rc/turn-on-linum-mode ()
  (interactive)
  (linum-mode 1))

(setq rc/linum-compatible-mode-hooks
      '(c-mode-common-hook
        emacs-lisp-mode-hook
        haskell-mode-hook
        perl-mode-hook
        python-mode-hook
        asm-mode-hook
        html-mode-hook
        css-mode-hook
        nxml-mode-hook))

(dolist (mode-hook rc/linum-compatible-mode-hooks)
  (add-hook mode-hook 'rc/turn-on-linum-mode))
