(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Documents/Personal/Notes.org")
         "* TODO %?\n  SCHEDULED: %t\n")))
(define-key global-map "\C-cc" 'org-capture)
