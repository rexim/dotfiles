(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun rc/guard-from-path (file-path stop-folder)
  (->> (split-string file-path "[/\\.]")
       (reverse)
       (-take-while (-lambda (x) (not (string= stop-folder x))))
       (-reduce-from (-lambda (acc x) (concat (upcase x) "_" acc)) "")))


(defun rc/update-include-guard (stop-folder)
  (interactive "sStop folder [src]: ")
  (let* ((file-path (buffer-file-name))
         (guard (rc/guard-from-path file-path (if (= 0 (length stop-folder))
                                                  "src"
                                                stop-folder))))
    (goto-char 0)
    (forward-word)
    (forward-char)
    (let ((begin (point)))
      (end-of-line)
      (let ((end (point)))
        (goto-char 0)
        (replace-string (buffer-substring begin end) guard)))))
