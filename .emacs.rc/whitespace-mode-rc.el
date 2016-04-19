(defun rc/turn-on-whitespace-mode ()
  (interactive)
  (whitespace-mode 1))

(defun rc/delete-trailing-whitespace-on-save ()
  (interactive)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(custom-set-variables
 '(whitespace-style (quote (face tabs spaces trailing space-before-tab
                                 newline indentation empty space-after-tab
                                 space-mark tab-mark newline-mark))))

(add-hook 'tuareg-mode-hook 'rc/turn-on-whitespace-mode)
(add-hook 'c++-mode-hook 'rc/turn-on-whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'rc/turn-on-whitespace-mode)

(add-hook 'tuareg-mode-hook 'rc/delete-trailing-whitespace-on-save)
