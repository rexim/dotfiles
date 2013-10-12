(rc/ensure-package-installed 'openwith)

(openwith-mode 1)

(when (eq system-type 'gnu/linux)
  (setq openwith-associations
        '(("\\.pdf\\'" "evince" (file))
          ("\\.djvu\\'" "evince" (file))
          ("\\.docx\\'" "libreoffice" (file))
          ("\\.doc\\'" "libreoffice" (file))
          ("\\.rtf\\'" "libreoffice" (file)))))
