(defvar thought-trap '())

(defconst thought-trap-list-buffer-name "*Thought Trap List*")

(defun thought-trap-put (thought)
  (interactive "sWhat are you thinking about? ")
  (setq thought-trap (cons thought thought-trap))
  (message "%s [Put]" thought))

(defun thought-trap-pop ()
  (interactive)
  (if (not thought-trap)
      (message "[Empty]")
    (message "%s [Remove]"(car thought-trap))
    (setq thought-trap (cdr thought-trap))))

(defun thought-trap-top ()
  (interactive)
  (if (not thought-trap)
      (message "[Empty]")
    (message (car thought-trap))))

(defun thought-trap-list ()
  (interactive)
  (let ((thought-trap-list-buffer (get-buffer-create thought-trap-list-buffer-name)))
    (with-current-buffer thought-trap-list-buffer
      (local-set-key (kbd "C-x t s") 'thought-trap-replace-from-current-buffer)
      (erase-buffer)
      (dolist (thought thought-trap)
        (insert thought)
        (insert "\n")))
    (switch-to-buffer thought-trap-list-buffer)))

(defun thought-trap-replace-from-current-buffer ()
  (interactive)
  (let ((new-thought-trap (split-string (buffer-string) "\n" t "\\s*")))
    (setq thought-trap new-thought-trap)
    (message "Modified Thought Trap")))

(global-set-key (kbd "C-x t p") 'thought-trap-put)
(global-set-key (kbd "C-x t q") 'thought-trap-pop)
(global-set-key (kbd "C-x t t") 'thought-trap-top)
(global-set-key (kbd "C-x t l") 'thought-trap-list)
