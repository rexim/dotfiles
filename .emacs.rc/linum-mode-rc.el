(defun rc/turn-on-linum-mode ()
  (interactive)
  (linum-mode 1))

(add-hook 'c-mode-common-hook   'rc/turn-on-linum-mode)
(add-hook 'emacs-lisp-mode-hook 'rc/turn-on-linum-mode)
