;;; TODO(c3bdae31-4329-4217-98a0-743b9dcbb6d2): extract autocommit into a separate package
;;;
;;; Once e266bfaa-2a01-4881-9e7f-ce2c592f7cdd is done, I think we can do that.

(defvar rc/autocommit-local-locks
  (make-hash-table :test 'equal))

(defun rc/file-truename-nilable (filename)
  (when filename
    (file-truename filename)))

(defun rc/autocommit--id ()
  (let ((id (-> default-directory
                (locate-dominating-file ".git")
                (rc/file-truename-nilable))))
    (when (not id)
      (error "%s is not inside of a git repository" default-directory))
    (unless (gethash id rc/autocommit-local-locks)
      (puthash id nil rc/autocommit-local-locks))
    id))

(defun rc/autocommit--get-lock (lock)
  (-> (rc/autocommit--id)
      (gethash rc/autocommit-local-locks)
      (plist-get lock)))

(defun rc/autocommit--set-lock (lock value)
  (puthash (rc/autocommit--id)
           (-> (rc/autocommit--id)
               (gethash rc/autocommit-local-locks)
               (plist-put lock value))
           rc/autocommit-local-locks))

(defun rc/autocommit--toggle-lock (lock)
  (-> lock
      (rc/autocommit--get-lock)
      (not)
      (rc/autocommit--set-lock)))

(defun rc/autocommit--create-dir-locals (file-name)
  (write-region "((nil . ((eval . (rc/autocommit-dir-locals)))))"
                nil file-name))

(defun rc/y-or-n-if (predicate question action)
  (when (or (not (funcall predicate))
            (y-or-n-p question))
    (funcall action)))

;;; TODO(4229cf9a-4768-4f5e-aca1-865256c64a23): rc/autocommit-init-dir should modify dir locals file on AST level
;;;
;;; Right know it just overrides .dir-locals file on text level. I
;;; want it to
;;; - read .dir-locals,
;;; - parse the assoc list,
;;; - check if there is already autocommit stuff
;;; - add autocommit stuff to the assoc list if needed
;;; - and write it back to the file
;;;
;;; That will enable us with modifying dir locals that contains custom
;;; stuff unrelated to autocommit
(defun rc/autocommit-init-dir (&optional dir)
  "Initialize autocommit folder."
  (interactive "DAutocommit directory: ")
  (let* ((autocommit-dir (if dir dir default-directory))
         (file-name (concat autocommit-dir
                            dir-locals-file)))
    (rc/y-or-n-if (-partial #'file-exists-p file-name)
                  (format "%s already exists. Replace it?" file-name)
                  (-partial #'rc/autocommit--create-dir-locals file-name))))

(defun rc/autocommit-dir-locals ()
  "The function that has to be put into the .dir-locals.el file
of the autocommit folder as evaluated for any mode."
  (interactive)
  (auto-revert-mode 1)
  (rc/autopull-changes)
  (add-hook 'after-save-hook
            'rc/autocommit-changes
            nil 'make-it-local))

;;; TODO: rc/toggle-autocommit-offline doesn't work correctly
;;;
;;; It should toggle offline for all of the folders at once
(defun rc/toggle-autocommit-offline ()
  "Toggle between OFFLINE and ONLINE modes.

Autocommit can be in two modes: OFFLINE and ONLINE. When ONLINE
rc/autocommit-changes does `git commit && git push'. When OFFLINE
rc/autocommit does only `git commit'."
  (interactive)
  (rc/autocommit--toggle-lock 'autocommit-offline)
  (if (rc/autocommit--get-lock 'autocommit-offline)
      (message "[OFFLINE] Autocommit Mode")
    (message "[ONLINE] Autocommit Mode")))

(defun rc/autopull-changes ()
  "Pull the recent changes.

Should be invoked once before working with the content under
autocommit. Usually put into the dir locals file."
  (interactive)
  (when (not (rc/autocommit--get-lock 'autopull-lock))
    (rc/autocommit--set-lock 'autopull-lock t)
    (if (rc/autocommit--get-lock 'autocommit-offline)
        (message "[OFFLINE] NOT Syncing the Agenda")
      (if (y-or-n-p (format "Sync the Agenda? [%s]" (rc/autocommit--id)))
          (progn
            (message (format "Syncing the Agenda [%s]" (rc/autocommit--id)))
            (shell-command "git pull"))
        (progn
          (rc/autocommit--set-lock 'autocommit-offline t)
          (message (format "[OFFLINE] NOT Syncing the Agenda [%s]"
                           (rc/autocommit--id))))))))

(defun rc/autocommit-changes ()
  "Commit all of the changes under the autocommit folder.

Should be invoked each time a change is made. Usually put into
dir locals file."
  (interactive)
  (if (rc/autocommit--get-lock 'autocommit-lock)
      (rc/autocommit--set-lock 'autocommit-changed t)
    (rc/autocommit--set-lock 'autocommit-lock t)
    (rc/autocommit--set-lock 'autocommit-changed nil)
    (set-process-sentinel (rc/run-commit-process (rc/autocommit--id))
                          (-partial 'rc/autocommit-beat (rc/autocommit--id)))))

(defun rc/run-commit-process (autocommit-directory)
  (let ((default-directory autocommit-directory))
    (let ((autocommit-message (format-time-string "Autocommit %s")))
      (start-process-shell-command
       (format "Autocommit-%s" autocommit-directory)
       (format "*Autocommit-%s*" autocommit-directory)
       (format (if (rc/autocommit--get-lock 'autocommit-offline)
                   "git add -A && git commit -m \"%s\""
                 "git add -A && git commit -m \"%s\" && git push origin master")
               autocommit-message)))))

(defun rc/autocommit-beat (autocommit-directory process event)
  (let ((default-directory autocommit-directory))
    (message (if (rc/autocommit--get-lock 'autocommit-offline)
                 "[OFFLINE] Autocommit: %s"
               "Autocommit: %s")
             event)
    (if (not (rc/autocommit--get-lock 'autocommit-changed))
        (rc/autocommit--set-lock 'autocommit-lock nil)
      (rc/autocommit--set-lock 'autocommit-changed nil)
      (set-process-sentinel (rc/run-commit-process autocommit-directory)
                            (-partial 'rc/autocommit-beat autocommit-directory)))))
