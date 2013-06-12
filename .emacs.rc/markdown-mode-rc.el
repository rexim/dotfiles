(rc/ensure-package-installed 'markdown-mode)

(when (eq system-type 'gnu/linux)
  (setq markdown-command "markdown_py"))
