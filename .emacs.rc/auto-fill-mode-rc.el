(defun rc/turn-on-auto-fill-mode ()
  (interactive)
  (auto-fill-mode 1))

(add-hook 'org-mode-hook 'rc/turn-on-auto-fill-mode)
(add-hook 'latex-mode-hook 'rc/turn-on-auto-fill-mode)
(add-hook 'markdown-mode-hook 'rc/turn-on-auto-fill-mode)
