(rc/require 'ggtags)

(defun rc/turn-on-ggtags ()
  (interactive)
  (ggtags-mode 1))

(add-hook 'c-mode-common-hook 'rc/turn-on-ggtags)
