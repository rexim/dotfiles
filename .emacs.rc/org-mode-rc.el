
(rc/require 'org-cliplink)

(global-set-key (kbd "C-x a") 'org-agenda)
(global-set-key (kbd "C-x p i") 'org-cliplink)

(setq org-agenda-files (list "~/Documents/Personal/Tasks.org"
                             "~/Documents/Personal/Imported.org"
                             "~/Documents/Personal/Itest.org"))

(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks 'invisible)

(defun rc/org-comment (&optional arg)
  (interactive "p")
  (kmacro-exec-ring-item (quote ([3 33 return 134217730 32 2] 0 "%d")) arg))

(global-set-key (kbd "C-x p c") 'rc/org-comment)

(defvar rc/autopull-lock nil)
(defvar rc/autocommit-lock nil)
(defvar rc/autocommit-changed nil)

(defun rc/autocommit-reset-locks ()
  (interactive)
  (setq rc/autocommit-lock nil)
  (setq rc/autocommit-changed nil))

(defun rc/autopull-changes ()
  (interactive)
  (when (not rc/autopull-lock)
    (setq rc/autopull-lock t)
    (async-shell-command "git pull")))

(defun rc/run-commit-process ()
  (let ((autocommit-message (format-time-string "Autocommit %s")))
    (start-process-shell-command "Autocommit"
                                 "*Autocommit*"
                                 (format "git add -u && git commit -m \"%s\" && git push origin master"
                                         autocommit-message))))

(defun rc/autocommit-beat (process event)
  (message "Autocommit is finished")
  (if (not rc/autocommit-changed)
      (setq rc/autocommit-lock nil)
    (setq rc/autocommit-changed nil)
    (set-process-sentinel (rc/run-commit-process)
                          'rc/autocommit-beat)))

(defun rc/autocommit-changes ()
  (interactive)
  (if rc/autocommit-lock
      (setq rc/autocommit-changed t)
    (setq rc/autocommit-lock t)
    (setq rc/autocommit-changed nil)
    (set-process-sentinel (rc/run-commit-process)
                          'rc/autocommit-beat)))

(defun rc/cliplink-task ()
  (interactive)
  (org-cliplink-retrieve-title (substring-no-properties (current-kill 0))
                               '(lambda (url title)
                                  (insert (concat "* TODO " title
                                                  "\n  [[" url "][" title "]]")))))

(global-set-key (kbd "C-x p t") 'rc/cliplink-task)

(setq org-agenda-custom-commands
      '(("u" "Unscheduled" tags "-imported+personal-SCHEDULED={.+}-DEADLINE={.+}/!+TODO"
         ((org-agenda-sorting-strategy '(priority-down))))
        ("p" "Personal" ((agenda "" ((org-agenda-tag-filter-preset (list "+personal"))))))
        ("w" "Work" ((agenda "" ((org-agenda-tag-filter-preset (list "+work"))))))
        ))
