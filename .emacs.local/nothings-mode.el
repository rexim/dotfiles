(require 'subr-x)

(defvar nothings-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++ style comments
	(modify-syntax-entry ?/ ". 124b" table)
	(modify-syntax-entry ?* ". 23" table)
	(modify-syntax-entry ?\n "> b" table)
    ;; Preprocessor stuff?
    (modify-syntax-entry ?# "." table)
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"" table)
    table))

(defun nothings-font-lock-keywords ()
  (list
   `("# *[a-zA-Z0-9_]+" . font-lock-preprocessor-face)
   `("#.*include \\(\\(<\\|\"\\).*\\(>\\|\"\\)\\)" . (1 font-lock-string-face))))

(define-derived-mode nothings-mode prog-mode "Nothings"
  "Simple major mode for editing C files."
  :syntax-table nothings-mode-syntax-table
  (setq-local font-lock-defaults '(nothings-font-lock-keywords))
  (setq-local comment-start "//"))

(provide 'nothings-mode)
