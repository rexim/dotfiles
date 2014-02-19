(defun rc/straight-string (s)
  (mapconcat '(lambda (x) x) (split-string s) " "))

(defun rc/extract-title-from-html (html)
  (let ((start (string-match "<title>" html))
        (end (string-match "</title>" html))
        (chars-to-skip (length "<title>")))
    (if (and start end (< start end))
        (substring html (+ start chars-to-skip) end)
      nil)))

(defun rc/prepare-cliplink-title (title)
  (let ((replace-table '(("\\[" . "{")
                         ("\\]" . "}")
                         ("&mdash;" . "—")))
        (max-length 77)
        (result (rc/straight-string title)))
    (dolist (x replace-table)
      (setq result (replace-regexp-in-string (car x) (cdr x) result)))
    (when (> (length result) max-length)
      (setq result (concat (substring result 0 max-length) "...")))
    result))

(defun rc/perform-cliplink (buffer url content)
  (let* ((decoded-content (decode-coding-string content 'utf-8))
         (title (rc/extract-title-from-html decoded-content)))
    (with-current-buffer buffer
      (if title
          (insert (format "[[%s][%s]]" url (rc/prepare-cliplink-title title)))
        (insert (format "[[%s]]" url))))))

(defun rc/cliplink ()
  (interactive)
  (let ((dest-buffer (current-buffer))
        (url (substring-no-properties (current-kill 0))))
    (url-retrieve
     url
     `(lambda (s)
        (rc/perform-cliplink ,dest-buffer ,url
                             (buffer-string))))))

(global-set-key (kbd "C-x p i") 'rc/cliplink)
