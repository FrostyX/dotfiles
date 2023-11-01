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


(ert-deftest test-foo ()
  (should (equal (+ 1 2) 4)))

(ert-deftest test-bar ()
  (should nil))

(ert-deftest test-human-friendly ()
  (should (equal (crontab-human-friendly) "At 04:05 on Sunday.")))
