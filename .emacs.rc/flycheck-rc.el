(rc/require 'flycheck)

(defun rc/turn-on-flycheck ()
  (interactive)
  (flycheck-mode 1))

(add-hook 'c++-mode-hook 'rc/turn-on-flycheck)
(add-hook 'c-mode-hook 'rc/turn-on-flycheck)
