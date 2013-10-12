(rc/ensure-package-installed 'openwith)

(openwith-mode 1)

(when (eq system-type 'gnu/linux)
  (setq openwith-associations
        '(("\\.pdf\\'" "evince" (file))
          ("\\.djvu\\'" "evince" (file))

          ;; FIXME(rexim): it's probably possible to merge this two
          ;; entries.
          ("\\.docx\\'" "libreoffice" (file))
          ("\\.doc\\'" "libreoffice" (file)))))
