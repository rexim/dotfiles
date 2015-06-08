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

;;; ac for clang config
;; (defun rc/ac-cc-mode-setup ()
;;   (add-to-list 'ac-sources 'ac-source-clang))

;; (add-hook 'c-mode-hook 'rc/ac-cc-mode-setup)
;; (add-hook 'c++-mode-hook 'rc/ac-cc-mode-setup)

;;; echo "" | g++ -v -x c++ -E -
;; (setq ac-clang-flags
;;       (mapcar (lambda (item)(concat "-I" item))
;;               (split-string
;;                "
;;  /usr/include/c++/4.9
;;  /usr/include/x86_64-linux-gnu/c++/4.9
;;  /usr/include/c++/4.9/backward
;;  /usr/lib/gcc/x86_64-linux-gnu/4.9/include
;;  /usr/local/include
;;  /usr/lib/gcc/x86_64-linux-gnu/4.9/include-fixed
;;  /usr/include/x86_64-linux-gnu
;;  /usr/include
;; "
;;                )))
