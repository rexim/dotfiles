(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (interactive)
             (setq tab-width 4
                   indent-tabs-mode nil)
             (auto-fill-mode 1)))
