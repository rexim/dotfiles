(defun rc/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(custom-set-variables
 '(whitespace-style (quote (face tabs spaces trailing space-before-tab
                                 newline indentation empty space-after-tab
                                 space-mark tab-mark))))

(let ((whitespace-enabled-modes
       '(
         tuareg-mode-hook
         c++-mode-hook
         c-mode-hook
         emacs-lisp-mode
         java-mode-hook
         lua-mode-hook
         rust-mode-hook
         scala-mode-hook
         markdown-mode-hook
         js2-mode-hook
         haskell-mode-hook
         python-mode-hook
         erlang-mode-hook
         asm-mode-hook
         nasm-mode-hook
         go-mode-hook
         nim-mode-hook
         )))
  (dolist (mode whitespace-enabled-modes)
    (add-hook mode 'rc/set-up-whitespace-handling)))
