(rc/require 'flycheck)

(defun rc/turn-on-flycheck ()
  (interactive)
  (flycheck-mode 1))

(defun rc/enable-c++11-flycheck ()
  (interactive)
  (setq flycheck-clang-language-standard "c++11")
  (setq flycheck-gcc-language-standard "c++11"))

(add-hook 'c++-mode-hook 'rc/turn-on-flycheck)
(add-hook 'c-mode-hook 'rc/turn-on-flycheck)
