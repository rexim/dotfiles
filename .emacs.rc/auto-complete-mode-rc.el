(rc/require 'auto-complete)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(defun rc/turn-on-auto-complete-mode ()
  (auto-complete-mode 1))

(add-hook 'c-mode-common-hook 'rc/turn-on-auto-complete-mode)
(add-hook 'emacs-lisp-mode-hook 'rc/turn-on-auto-complete-mode)
