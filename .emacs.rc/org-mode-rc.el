(rc/require 'org-cliplink)

(global-set-key (kbd "C-c a l") 'org-agenda-list)
(global-set-key (kbd "C-c a s") 'org-search-view)
(global-set-key (kbd "C-x p i") 'org-cliplink)

(setq org-agenda-files (list "~/Documents/Personal/Tasks.org"
                             "~/Documents/Personal/Imported.org"
                             "~/Documents/Personal/Archive.org"))
