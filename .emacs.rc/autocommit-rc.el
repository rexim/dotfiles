;; TODO(0b04f219-6c37-4811-898f-e9252f52c3f3): interactive function for creating dir local variables file
;; Subtasks:
;; - [ ] 845361b1-a0e8-4f1d-89c8-2f7b4d0c92f8
;; - [ ] 15404e89-5246-4a31-94a1-81d00c066a74
;; - [ ] 94f35abc-e2d8-4c3c-a2f9-8bdfcf4f6586
;; - [ ] 4309af7f-53bf-4ef3-a668-67c38e8a4be5
;;
;; This is how dir local vars for autocommit look like:
;; ((nil . ((eval . (rc/autocommit-dir-locals)))))
;;
;; I think such init function should also create gitignores. Or at
;; least append it with it's own stuff.
;;
;; Useful links:
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Local-Variables.html

(defvar rc/autocommit-offline nil)
(defvar rc/autopull-lock nil)
(defvar rc/autocommit-lock nil)
(defvar rc/autocommit-changed nil)

;;; TODO(15404e89-5246-4a31-94a1-81d00c066a74): implement ask-if-exists
;;;
;;; Parent: 0b04f219-6c37-4811-898f-e9252f52c3f3
(defun rc/autocommit--ask-if-exists (file-name question)
  )

;;; TODO(94f35abc-e2d8-4c3c-a2f9-8bdfcf4f6586): implement create-dir-locals
;;;
;;; Parent: 0b04f219-6c37-4811-898f-e9252f52c3f3
(defun rc/autocommit--create-dir-locals (dir-locals-file-name)
  )

;;; TODO(4309af7f-53bf-4ef3-a668-67c38e8a4be5): implement print-filename
;;;
;;; Parent: 0b04f219-6c37-4811-898f-e9252f52c3f3
(defun rc/autocommit--print-file-name (file-name)
  )

;;; TODO(845361b1-a0e8-4f1d-89c8-2f7b4d0c92f8): optional dir argument for rc/autocommit-init-dir
;;;
;;; Parent: 0b04f219-6c37-4811-898f-e9252f52c3f3
(defun rc/autocommit-init-dir ()
  "Initialize autcommit folder."
  (interactive)
  (-> default-directory
      (concat dir-locals-file)
      (rc/autocommit--ask-if-exists "Do you really wanna replace it?")
      (rc/autocommit--create-dir-locals)
      (rc/autocommit--print-file-name)))

(defun rc/autocommit-dir-locals ()
  "The function that has to be put into the .dir-locals.el file
of the autocommit folder as evaluated for any mode."
  (interactive)
  (auto-revert-mode 1)
  (rc/autopull-changes)
  (add-hook 'after-save-hook
            'rc/autocommit-changes
            nil 'make-it-local))

(defun rc/toggle-autocommit-offline ()
  "Toggle between OFFLINE and ONLINE modes.

Autocommit can be in two modes: OFFLINE and ONLINE. When ONLINE
rc/autocommit-changes does `git commit && git push'. When OFFLINE
rc/autocommit does only `git commit'."
  (interactive)
  (setq rc/autocommit-offline (not rc/autocommit-offline))
  (if rc/autocommit-offline
      (message "[OFFLINE] Autocommit Mode")
    (message "[ONLINE] Autocommit Mode")))

(defun rc/autocommit-reset-locks ()
  "Reset all of the autocommit locks.

Autocommit is asynchronous and to perform its job without any
race conditions it maintains a set of internal locks. If this set
goes into an incosistent state you can reset them with this
function."
  (interactive)
  (setq rc/autocommit-lock nil)
  (setq rc/autocommit-changed nil))

(defun rc/autopull-changes ()
  "Pull the recent changes.

Should be invoked once before working with the content under
autocommit. Usually put into the dir locals file."
  (interactive)
  (when (not rc/autopull-lock)
    (setq rc/autopull-lock t)
    (if rc/autocommit-offline
        (message "[OFFLINE] NOT Syncing the Agenda")
      (if (y-or-n-p "Sync the Agenda?")
          (progn
            (message "Syncing the Agenda")
            (shell-command "git pull"))
        (progn
          (setq rc/autocommit-offline t)
          (message "[OFFLINE] NOT Syncing the Agenda"))))))

(defun rc/autocommit-changes ()
  "Commit all of the changes under the autocommit folder.

Should be invoked each time a change is made. Usually put into
dir locals file."
  (interactive)
  (if rc/autocommit-lock
      (setq rc/autocommit-changed t)
    (setq rc/autocommit-lock t)
    (setq rc/autocommit-changed nil)
    (set-process-sentinel (rc/run-commit-process)
                          'rc/autocommit-beat)))

(defun rc/run-commit-process ()
  (let ((autocommit-message (format-time-string "Autocommit %s")))
    (let ((default-directory "~/Documents/Agenda/"))
      (start-process-shell-command
       "Autocommit"
       "*Autocommit*"
       (format (if rc/autocommit-offline
                   "git add -A && git commit -m \"%s\""
                 "git add -A && git commit -m \"%s\" && git push origin master")
               autocommit-message)))))

(defun rc/autocommit-beat (process event)
  (message (if rc/autocommit-offline
               "[OFFLINE] Autocommit: %s"
             "Autocommit: %s")
           event)
  (if (not rc/autocommit-changed)
      (setq rc/autocommit-lock nil)
    (setq rc/autocommit-changed nil)
    (set-process-sentinel (rc/run-commit-process)
                          'rc/autocommit-beat)))
