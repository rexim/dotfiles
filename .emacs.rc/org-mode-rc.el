(global-set-key (kbd "C-x a") 'org-agenda)

(setq org-agenda-files (list "~/Documents/Agenda/"))

(custom-set-variables
 '(org-modules
   (quote
    (org-bbdb
     org-bibtex
     org-docview
     org-gnus
     org-habit
     org-info
     org-irc
     org-mhe
     org-rmail
     org-w3m)))
 '(org-enforce-todo-dependencies t)
 '(org-agenda-dim-blocked-tasks (quote invisible))
 '(org-agenda-exporter-settings
   (quote ((org-agenda-tag-filter-preset (list "+personal"))))))

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
    (let ((default-directory "~/Documents/Agenda/"))
      (start-process-shell-command "Autocommit"
                                   "*Autocommit*"
                                   (format "git add -u && git commit -m \"%s\" && git push origin master"
                                           autocommit-message)))))

(defun rc/autocommit-beat (process event)
  (message "Autocommit: %s" event)
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
  (org-cliplink-retrieve-title
   (substring-no-properties (current-kill 0))
   '(lambda (url title)
      (insert (if title
                  (concat "* TODO " title
                          "\n  [[" url "][" title "]]")
                (concat "* TODO " url
                        "\n  [[" url "]]"))))))

(global-set-key (kbd "C-x p t") 'rc/cliplink-task)

(defun rc/org-get-heading-name ()
  (nth 4 (org-heading-components)))

(defun rc/org-kill-heading-name-save ()
  (interactive)
  (let ((heading-name (rc/org-get-heading-name)))
    (kill-new heading-name)
    (message "Kill \"%s\"" heading-name)))

(global-set-key (kbd "C-x p w") 'rc/org-kill-heading-name-save)

(setq org-agenda-custom-commands
      '(("u" "Unscheduled" tags "+personal-SCHEDULED={.+}-DEADLINE={.+}/!+TODO"
         ((org-agenda-sorting-strategy '(priority-down))))
        ("p" "Personal" ((agenda "" ((org-agenda-tag-filter-preset (list "+personal"))))))
        ("w" "Work" ((agenda "" ((org-agenda-tag-filter-preset (list "+work"))))))
        ))
