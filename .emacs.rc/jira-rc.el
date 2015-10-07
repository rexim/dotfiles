(defun rc/markdown-linkify-jira-tasks (start end)
  (interactive "r")
  (replace-regexp "\\([A-Z]+-[0-9]+\\)"
                  "[\\1](https://jira.spirenteng.com/browse/\\1)"
                  nil start end))

(defvar rc/issues-folder "/home/rexim/Programming/itest/Issues")

(defun rc/extract-jira-number (jira-id)
  (string-match "[A-Z]+-\\([0-9]+\\)" jira-id)
  (match-string 1 jira-id))

(defun rc/open-issue-folder (start end)
  (interactive "r")
  (find-file
   (concat rc/issues-folder
           "/*"
           (rc/extract-jira-number (buffer-substring start end))
           "*")
   t))

(global-set-key (kbd "C-c i o") 'rc/open-issue-folder)
