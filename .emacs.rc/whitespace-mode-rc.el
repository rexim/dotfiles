(defun rc/turn-on-whitespace-mode ()
  (interactive)
  (whitespace-mode 1))

(defun rc/delete-trailing-whitespace-on-save ()
  (interactive)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(defun rc/set-up-whitespace-handling ()
  (interactive)
  (rc/turn-on-whitespace-mode)
  (rc/delete-trailing-whitespace-on-save))

(custom-set-variables
 '(whitespace-style (quote (face tabs spaces trailing space-before-tab
                                 newline indentation empty space-after-tab
                                 space-mark tab-mark newline-mark))))

(add-hook 'tuareg-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c++-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'java-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'rc/set-up-whitespace-handling)
