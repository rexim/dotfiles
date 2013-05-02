(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (interactive)
             (setq tab-width 4
                   indent-tabs-mode nil)
             (auto-fill-mode 1)))
