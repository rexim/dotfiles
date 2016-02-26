(rc/require 'org-cliplink)

(when (eq system-type 'gnu/linux)
  (custom-set-variables
   '(org-cliplink-transport-implementation (quote curl))))

(global-set-key (kbd "C-x p i") 'org-cliplink)
