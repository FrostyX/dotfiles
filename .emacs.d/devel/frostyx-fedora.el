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


;; TODO frostyx/copr-build
;; It will upload the current .spec or SRPM file to Copr
;; It will pop up helm with all my projects to select in which project
;; is should be built
