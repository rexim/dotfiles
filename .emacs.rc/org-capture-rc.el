(setq org-capture-templates
      '(("t" "Add Personal task" entry (file "~/Documents/Personal/Tasks.org")
         "* TODO %?\n  SCHEDULED: %t\n")
        ("i" "Add iTest task" entry (file "~/Documents/Personal/Itest.org")
         "* TODO %?\n  SCHEDULED: %t\n")))
(define-key global-map "\C-cc" 'org-capture)
