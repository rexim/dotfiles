(defun rc/turn-on-linum-mode ()
  (interactive)
  (linum-mode 1))

(setq rc/linum-compatible-modes
      '(c-mode-common-hook
        emacs-lisp-mode-hook
        haskell-mode-hook
        perl-mode-hook
        python-mode-hook
        asm-mode-hook
        html-mode-hook
        css-mode-hook))

(dolist (mode rc/linum-compatible-modes)
  (add-hook mode 'rc/turn-on-linum-mode))
