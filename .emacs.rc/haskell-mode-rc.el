(rc/require 'haskell-mode)

(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)

(defun haskell-process-load-complete (session process buffer reload module-buffer &optional cont)
  "Handle the complete loading response. BUFFER is the string of
text being sent over the process pipe. MODULE-BUFFER is the
actual Emacs buffer of the module being loaded."
  (when (get-buffer (format "*%s:splices*" (haskell-session-name session)))
    (with-current-buffer (haskell-interactive-mode-splices-buffer session)
      (erase-buffer)))
  (let* ((ok (cond
              ((haskell-process-consume
                process
                "Ok, \\(?:[0-9]+\\) modules? loaded\\.$")
               t)
               ((haskell-process-consume
                process
                "Ok, \\(?:[a-z]+\\) module loaded\\.$") ;; for ghc 8.4
               t)
              ((haskell-process-consume
                process
                "Failed, \\(?:[0-9]+\\) modules? loaded\\.$")
               nil)
              ((haskell-process-consume
                process
                "Ok, modules loaded: \\(.+\\)\\.$")
               t)
              ((haskell-process-consume
                process
                "Failed, modules loaded: \\(.+\\)\\.$")
               nil)
              (t
               (error (message "Unexpected response from haskell process.")))))
         (modules (haskell-process-extract-modules buffer))
         (cursor (haskell-process-response-cursor process))
         (warning-count 0))
    (haskell-process-set-response-cursor process 0)
    (haskell-check-remove-overlays module-buffer)
    (while
        (haskell-process-errors-warnings module-buffer session process buffer)
      (setq warning-count (1+ warning-count)))
    (haskell-process-set-response-cursor process cursor)
    (if (and (not reload)
             haskell-process-reload-with-fbytecode)
        (haskell-process-reload-with-fbytecode process module-buffer)
      (haskell-process-import-modules process (car modules)))
    (if ok
        (haskell-mode-message-line (if reload "Reloaded OK." "OK."))
      (haskell-interactive-mode-compile-error session "Compilation failed."))
    (when cont
      (condition-case-unless-debug e
          (funcall cont ok)
        (error (message "%S" e))
        (quit nil)))))
