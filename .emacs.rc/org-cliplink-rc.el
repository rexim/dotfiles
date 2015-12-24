(rc/require 'org-cliplink)

(custom-set-variables
 '(org-cliplink-transport-implementation (quote curl)))

(global-set-key (kbd "C-x p i") 'org-cliplink)
