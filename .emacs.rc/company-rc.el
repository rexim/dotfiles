(rc/require 'company)
(rc/require 'company-lua)

(require 'company)
(require 'company-lua)

(defun rc/lua-mode-company-init ()
  (interactive)
  (setq-local company-backends '((company-lua
                                  company-etags
                                  company-dabbrev-code
                                  company-yasnippet)))
  (setq-local company-lua-interpreter 'love))

(add-hook 'lua-mode-hook 'rc/lua-mode-company-init)

(global-company-mode)

(add-hook 'tuareg-mode-hook
          (lambda ()
            (interactive)
            (company-mode 0)))
