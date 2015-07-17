(defvar thought-stack '())

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

(global-set-key (kbd "C-x t p") 'thought-stack-put)
(global-set-key (kbd "C-x t q") 'thought-stack-pop)
(global-set-key (kbd "C-x t t") 'thought-stack-top)
