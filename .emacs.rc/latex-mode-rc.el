(add-hook 'tex-mode-hook (lambda ()
                           (interactive)
                           (add-to-list 'tex-verbatim-environments "code")))

