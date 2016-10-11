(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun rc/guard-from-path (file-path)
  (let* ((cmps (-> file-path
                   (split-string "[/\\.]")
                   (reverse)))
         (result nil)
         (guard ""))
    (while (and cmps
                (not (string= (car cmps) "src")))
      (push (car cmps) result)
      (pop cmps))

    (while result
      (setq guard (concat guard (car result) "_"))
      (pop result))

    (upcase guard)))

(defun rc/update-include-guard ()
  (interactive)
  (let* ((file-path (buffer-file-name))
         (guard (rc/guard-from-path file-path)))
    (goto-char 0)
    (forward-word)
    (forward-char)
    (let ((begin (point)))
      (end-of-line)
      (let ((end (point)))
        (goto-char 0)
        (replace-string (buffer-substring begin end) guard)))))
