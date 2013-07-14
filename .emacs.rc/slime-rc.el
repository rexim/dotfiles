(rc/ensure-package-installed 'slime)
(require 'slime-autoloads)
;; WARNING! If you're experiencing a problem with initialization of
;; SLIME, remove all its *.elc files.
(slime-setup '(slime-fancy slime-banner))
(setq inferior-lisp-program "/usr/bin/sbcl")
