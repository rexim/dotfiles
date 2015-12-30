(setq org-capture-templates
      '(("p" "Add Personal task" entry (file "~/Documents/Personal/Tasks.org")
         "* TODO %?\n  SCHEDULED: %t\n")
        ("i" "Add iTest task" entry (file "~/Documents/Personal/Itest.org")
         "* TODO %?\n  SCHEDULED: %t\n")
        ("K" "Personal Cliplink" entry (file "~/Documents/Personal/Tasks.org")
         "* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)))
(define-key global-map "\C-cc" 'org-capture)
