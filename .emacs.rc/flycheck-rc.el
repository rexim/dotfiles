(rc/require 'flycheck)

(defun rc/turn-on-flycheck ()
  (flycheck-mode 1))

(add-hook 'c++-mode 'rc/turn-on-flycheck)
(add-hook 'c-mode 'rc/turn-on-flycheck)
