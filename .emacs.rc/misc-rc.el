(global-set-key (kbd "C-c <C-left>")  'windmove-left)
(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-up>")    'windmove-up)
(global-set-key (kbd "C-c <C-down>")  'windmove-down)

(setq-default inhibit-splash-screen t
              make-backup-files nil
              tab-width 4
              indent-tabs-mode nil
              compilation-scroll-output t)

;;; Taken from here:
;;; http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
(defun rc/put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message filename))))

(defun rc/put-buffer-name-on-clipboard ()
  "Put the current buffer name on the clipboard"
  (interactive)
  (kill-new (buffer-name))
  (message (buffer-name)))

(defun rc/kill-autoloads-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (string-match-p "-autoloads.el" name)
        (kill-buffer buffer)
        (message "Killed autoloads buffer %s" name)))))

(defun rc/url-from-clipboard-to-org-link ()
  (interactive)
  (let ((dest-buffer (current-buffer))
        (url (substring-no-properties (current-kill 0))))
    (url-retrieve
     url
     `(lambda (s)
        (goto-char (point-max))
        (search-backward-regexp "<title>[[:space:]\n]*\\(.*\\)[[:space:]\n]*</title>")
        (let ((title (match-string 1)))
          (with-current-buffer ,dest-buffer
            (insert (format "[[%s][%s]]" ,url title))))))))
