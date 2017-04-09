(setq org-capture-templates
      '(("p" "Capture task" entry (file "~/Documents/Agenda/Tasks.org")
         "* TODO %?\n  SCHEDULED: %t\n")
        ("K" "Cliplink capture task" entry (file "~/Documents/Agenda/Tasks.org")
         "* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)))
(define-key global-map "\C-cc" 'org-capture)
