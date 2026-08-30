;; TODO: support Extended HUID \[0-9]{8}-[0-9]{6}(-[a-zA-Z0-9\\-]*)?\
(defconst tatr-default-priority 100)
(defconst tatr-default-tags (list "scope"))

(defun tatr-new-huid ()
  (format-time-string "%Y%m%d-%H%M%S" nil t))

(defun tatr-huid-p (huid)
  (when (string-match "^[0-9]\\{8\\}-[0-9]\\{6\\}$" huid) t))

(defun tatr-find-database ()
  (let ((dir default-directory))
    (catch 'result
      (while dir
        (let ((db-dir (file-name-concat dir "tasks")))
          (if (file-directory-p db-dir)
              (throw 'result db-dir)
            (setq dir (file-name-parent-directory dir))))))))

(defun tatr-create (title &optional huid)
  (let ((db-dir (tatr-find-database)))
    (if (not db-dir)
        (not (message "tasks/ folder was not found"))
      (let* ((huid (if huid huid (tatr-new-huid)))
             (task-path (file-name-concat db-dir huid)))
        (if (file-exists-p task-path)
            (not (message "ERROR: %s already exists." task-path))
          (mkdir task-path)
          (let ((task-md-path (file-name-concat task-path "TASK.md"))
                (task-md-content (format (concat "# %s\n"
                                                 "\n"
                                                 "- STATUS: OPEN\n"
                                                 "- PRIORITY: %s\n"
                                                 "- TAGS: %s\n")
                                         title
                                         tatr-default-priority
                                         (mapconcat #'identity tatr-default-tags))))
            (write-region task-md-content nil task-md-path)
            (cons huid task-md-path)))))))

(defun tatr-create-from-title (title)
  (interactive "STitle: \n")
  (let ((task (tatr-create title)))
    (when task
      (let ((task-md-path (cdr task)))
        (find-file-other-window task-md-path)))))

(defun tatr-create-from-todo-at-point ()
  (interactive)
  (let ((line (thing-at-point 'line)))
    (cond
     ((string-match "\\(.*\\)TODO:\\(.*\\)" line)
      (let* ((prefix (match-string 1 line))
             (title (string-trim (match-string 2 line)))
             (task (tatr-create title)))
        (when task
          (let ((huid (car task))
                (task-md-path (cdr task)))
            (delete-line)
            (insert (format "%sTASK(%s): %s\n" prefix huid title))
            (find-file-other-window task-md-path)))))
     ((string-match (concat "\\(.*\\)TODO("       ; prefix
                            "\\([0-9]\\{4\\}\\)-" ; year
                            "\\([0-9]\\{2\\}\\)-" ; month
                            "\\([0-9]\\{2\\}\\) " ; day
                            "\\([0-9]\\{2\\}\\):" ; hour
                            "\\([0-9]\\{2\\}\\):" ; minute
                            "\\([0-9]\\{2\\}\\)"  ; second
                            "):\\(.*\\)")         ; title
                    line)
      (let* ((prefix (match-string 1 line))
             (year   (match-string 2 line))
             (month  (match-string 3 line))
             (day    (match-string 4 line))
             (hour   (match-string 5 line))
             (minute (match-string 6 line))
             (second (match-string 7 line))
             (title  (string-trim (match-string 8 line)))
             (huid   (format "%s%s%s-%s%s%s" year month day hour minute second))
             (task   (tatr-create title huid)))
        (when task
          (let ((task-md-path (cdr task)))
            (delete-line)
            (insert (format "%sTASK(%s): %s\n" prefix huid title))
            (find-file-other-window task-md-path)))))
     (t (not (message "No TODO under cursor"))))))

(defun tatr-find-by-huid ()
  (interactive)
  (let ((huid
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (completing-read "HUID: " nil)))
        (db-dir (tatr-find-database)))
    (if (not (tatr-huid-p huid))
        (not (message "%s is not a valid HUID" huid))
      (if (not db-dir)
          (not (message "tasks/ folder was not found"))
        (let ((task-md-path (file-name-concat db-dir huid "TASK.md")))
          (if (not (file-regular-p task-md-path))
              (not (message "Task %s was not found" huid))
            (find-file-other-window task-md-path)))))))

(defun tatr-grep-referers ()
  (interactive)
  (if (not (string-match "\\(.*\\)/tasks/\\([0-9]\\{8\\}-[0-9]\\{6\\}\\)/" default-directory))
      (not (message "You are not in a task folder"))
    (let ((project-root (match-string 1 default-directory))
          (huid (match-string 2 default-directory)))
      (rgrep huid "*" project-root))))

(defun tatr-copy-huid-to-clipboard ()
  (interactive)
  (if (not (string-match "\\(.*\\)/tasks/\\([0-9]\\{8\\}-[0-9]\\{6\\}\\)/" default-directory))
      (not (message "You are not in a task folder"))
    (let ((huid (match-string 2 default-directory)))
      (kill-new huid)
      (message "Copied %s to clipboard" huid))))

(provide 'tatr)
