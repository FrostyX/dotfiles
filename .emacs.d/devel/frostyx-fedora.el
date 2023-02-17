(defun frostyx/rpmdev-bumpspec ()
  (interactive)
  (shell-command
   (string-join (list "rpmdev-bumpspec" buffer-file-name) " ")))


(defun frostyx/rpmdev-bumpspec-new-version (version)
  (interactive "sNew version: ")
  (shell-command
   (string-join
    (list "rpmdev-bumpspec"
          buffer-file-name
          "--new"
          version)
    " ")))
