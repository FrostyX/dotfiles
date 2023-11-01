;; Run the tests via `M-x ert' in Emacs
;;;
;; Or run them in the commandline:
;;     emacs -batch -l ert -l test-crontab.el -f  ert-run-tests-batch-and-exit


(let* ((parent (file-name-directory
               (directory-file-name
                (file-name-directory (buffer-file-name)))))
       (file "frostyx-crontab.el")
       (path (expand-file-name file parent)))
  (load-file path))


(ert-deftest test-human-friendly-simple ()
  (should (equal (crontab-human-friendly "5 4 * * 7")
                 "At 04:05 on Sunday."))

  (should (equal (crontab-human-friendly "15 14 1 * *")
                 "At 14:15 on day-of-month 1."))

  (should (equal (crontab-human-friendly "5 0 * 8 *")
                 "At 00:05 in August."))

  (should (equal (crontab-human-friendly "15 14 1 * *")
                 "At 14:15 on day-of-month 1.")))


(ert-deftest test-human-friendly-advanced ()
  (should (equal (crontab-human-friendly "0 22 * * 1-5")
                 "At 22:00 on every day-of-week from Monday through Friday."))

  (should (equal (crontab-human-friendly "23 0-20/2 * * *")
                 "At minute 23 past every 2nd hour from 0 through 20."))

  (should (equal (crontab-human-friendly "0 0,12 1 */2 *")
                 "At minute 0 past hour 0 and 12 on day-of-month 1 in every 2nd month."))

  (should (equal (crontab-human-friendly "0 4 8-14 * *")
                 "At 04:00 on every day-of-month from 8 through 14."))

  (should (equal (crontab-human-friendly "0 0 1,15 * 3")
                 "At 00:00 on day-of-month 1 and 15 and on Wednesday."))

  (should (equal (crontab-human-friendly "@weekly")
                 "At 00:00 on Sunday."))

  (should (equal (crontab-human-friendly "0 22 * * 1-5")
                 "At 22:00 on every day-of-week from Monday through Friday."))

  (should (equal (crontab-human-friendly "23 0-20/2 * * *")
                 "At minute 23 past every 2nd hour from 0 through 20.")))
