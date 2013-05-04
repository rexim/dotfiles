(rc/ensure-package-installed 'openwith)

(openwith-mode 1)

(when (eq system-type 'gnu/linux)
  (setq openwith-associations
        '(("\\.pdf\\'" "evince" (file)))))
