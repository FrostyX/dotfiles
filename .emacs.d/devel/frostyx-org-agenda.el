(defvar doom-modeline--frostyx-org-agenda-count "")
(doom-modeline-def-segment frostyx-org-agenda-count
  (when (equal major-mode 'org-agenda-mode)
    (if (string= doom-modeline--frostyx-org-agenda-count "")
        (doom-modeline--frostyx-org-agenda-update))
    (format-mode-line doom-modeline--frostyx-org-agenda-count)))


(defvar doom-modeline--frostyx-org-agenda-timer nil)
(doom-modeline-add-variable-watcher
 'doom-modeline-frostyx-org-agenda
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-frostyx-org-agenda val)
     (doom-modeline-frostyx-org-agenda-timer))))


(defvar doom-modeline--frostyx-org-agenda-timer nil)
(defun doom-modeline-frostyx-org-agenda-timer ()
  (if (timerp doom-modeline--frostyx-org-agenda-timer)
      (cancel-timer doom-modeline--frostyx-org-agenda-timer))
  (setq doom-modeline--frostyx-org-agenda-timer
        (and doom-modeline-frostyx-org-agenda
             (run-with-idle-timer
              10 10 #'doom-modeline--frostyx-org-agenda-update))))

(defvar doom-modeline-frostyx-org-agenda t)
(doom-modeline-frostyx-org-agenda-timer)


(defun doom-modeline--frostyx-org-agenda-update ()
  (when doom-modeline-frostyx-org-agenda
    (let ((soon-count (length (org-ql-select (org-agenda-files)
                                '(priority "3")
                                :action #'org-get-heading)))
          (inbox-count (length (org-ql-query
                                 :from "~/Org/gtd/inbox.org"
                                 :select #'org-get-heading))))
      (setq doom-modeline--frostyx-org-agenda-count
            (format "Also %s tasks in the inbox and %s tasks for next week"
                    ;; TODO Don't abuse circe face here
                    (propertize (number-to-string inbox-count)
                                'face 'circe-tracking-channel-face)
                    (propertize (number-to-string soon-count)
                                'face 'circe-tracking-channel-face))))))
