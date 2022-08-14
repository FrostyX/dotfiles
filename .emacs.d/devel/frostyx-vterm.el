(defun frostyx/projectile-run-vterm (name &optional project-root)
  "Run a named multi-vterm.
If the vterm instance with the same name exists within the
project, only switch to its buffer."
  (let* ((project-root (or project-root (projectile-acquire-root)))
         (project-name (projectile-project-name project-root))
         (buffer-name (concat project-name " - " name))
         (full-buffer-name
          (concat "*" multi-vterm-buffer-name
                  " - " buffer-name "*")))

    (unless (require 'multi-vterm nil 'noerror)
      (error "Package 'multi-vterm' is not available"))

    (unless (buffer-live-p (get-buffer full-buffer-name))
      (projectile-with-default-dir project-root
        (multi-vterm))
      (multi-vterm-rename-buffer buffer-name))

    (switch-to-buffer full-buffer-name)))


(defun frostyx/vterm-buffers ()
  (mapcar #'buffer-name
          (cl-remove-if-not
           (lambda (buf)
             (with-current-buffer buf
               (derived-mode-p 'vterm-mode)))
          (buffer-list))))


(defun frostyx/projectile-vterm-buffers ()
  (let ((project-root (projectile-acquire-root)))
    (if project-root
        (cl-intersection
         (frostyx/vterm-buffers)
         (mapcar #'buffer-name
                 (projectile-project-buffers
                  (expand-file-name project-root)))))))


(defun frostyx/switch-to-vterm-buffer ()
  (interactive)
  (let* ((options (frostyx/vterm-buffers))
         (result (completing-read "Vterm: " options)))
    (switch-to-buffer result)))


(defun frostyx/projectile-switch-to-vterm-buffer ()
  (interactive)
  (let* ((options (frostyx/projectile-vterm-buffers))
         (result (completing-read "Vterm: " options)))
    (switch-to-buffer result)))


;; Usage
;; (frostyx/projectile-run-vterm "barr" "~/git/dotfiles")
