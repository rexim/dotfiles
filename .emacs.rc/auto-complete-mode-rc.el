(rc/require 'auto-complete)
(rc/require 'ac-haskell-process)
(rc/require 'auto-complete-clang)

(require 'auto-complete)
(require 'auto-complete-config)
(require 'ac-haskell-process)
(require 'auto-complete-clang)

(ac-config-default)

(global-auto-complete-mode 1)

(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-interactive-hook 'ac-haskell-process-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'haskell-interactive-mode))
