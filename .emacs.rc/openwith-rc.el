(rc/require 'openwith)

(openwith-mode 1)


(setq openwith-associations
      (append
       (when (eq system-type 'gnu/linux)
         '(("\\.pdf\\'" "evince" (file))
           ("\\.djvu\\'" "evince" (file))))
       '(("\\.docx\\'" "libreoffice" (file))
         ("\\.xlsx\\'" "libreoffice" (file))
         ("\\.doc\\'" "libreoffice" (file))
         ("\\.rtf\\'" "libreoffice" (file))
         ("\\.ods\\'" "libreoffice" (file))
         ("\\.mp3\\'" "vlc" (file)))))
