(defvar thought-stack '())

(defconst thought-stack-list-buffer-name "*Thought Stack List*")

(defun thought-stack-put (thought)
  (interactive "sWhat are you thinking about? ")
  (setq thought-stack (cons thought thought-stack))
  (message "%s [Put]" thought))

(defun thought-stack-pop ()
  (interactive)
  (if (not thought-stack)
      (message "[Empty]")
    (message "%s [Remove]"(car thought-stack))
    (setq thought-stack (cdr thought-stack))))

(defun thought-stack-top ()
  (interactive)
  (if (not thought-stack)
      (message "[Empty]")
    (message (car thought-stack))))

(defun thought-stack-list ()
  (interactive)
  (let ((thought-stack-list-buffer (get-buffer-create thought-stack-list-buffer-name)))
    (with-current-buffer thought-stack-list-buffer
      (erase-buffer)
      (dolist (thought thought-stack)
        (insert "* TODO ")
        (insert thought)
        (insert "\n")))
    (switch-to-buffer thought-stack-list-buffer)))

(global-set-key (kbd "C-x t p") 'thought-stack-put)
(global-set-key (kbd "C-x t q") 'thought-stack-pop)
(global-set-key (kbd "C-x t t") 'thought-stack-top)
(global-set-key (kbd "C-x t l") 'thought-stack-list)
