(add-to-list 'auto-mode-alist '("\\.qml\\'" . js-mode))
(add-hook 'js-mode-hook
          '(lambda ()
             (setq indent-tabs-mode (getenv "WORK_4GEO"))))
