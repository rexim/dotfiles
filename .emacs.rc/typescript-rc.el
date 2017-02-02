(rc/require 'typescript-mode)

(add-to-list 'compilation-error-regexp-alist
             '("ERROR in \\(.*\\)\n(\\([0-9]+\\),\\([0-9]+\\)):" 1 2 3))
