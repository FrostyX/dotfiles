;; TODO Make name optional and interactively read it
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
  (let ((project-root (string-remove-suffix
                       "/" (projectile-acquire-root))))
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


(setq frostyx-initial-vterms
      '(
        ;; TODO What about non-git terminals
        ;; (misc . (ansible maint))))
        ;; (hacking . (hacking blog))

        (nil . ("hacking" "ansible"))

        ("~/git/copr" . ("copr" "frontend" "backend"))))


(defun frostyx/run-initial-vterms ()
  (interactive)
  (let ((current-buffer (current-buffer)))
    (dolist (tuple frostyx-initial-vterms)
      (dolist (vterm-name (cdr tuple))
        (frostyx/projectile-run-vterm vterm-name (car tuple))))
        ;; (let (project-root (car tuple))
        ;;   (if project-root
        ;;       (frostyx/projectile-run-vterm vterm-name project-root)
        ;;     (progn
        ;;       (multi-vterm)
        ;;       (multi-vterm-rename-buffer vterm-name))))))
    (switch-to-buffer current-buffer)))


;; TODO

;; (defun frostyx/projectile-run-vterm-for-virtual-project ()
;;   (make-empty-file "/home/jkadlcik/.emacs.d/vprojects/misc/.projectile" t)
;;   (frostyx/projectile-run-vterm "III" "/home/jkadlcik/.emacs.d/vprojects/misc/")

;;   (vterm-insert "cd")
;;   (vterm-send-return)
;;   (vterm-clear))




(evil-define-motion vterm-evil-next-line (count)
  "Move the cursor COUNT lines down.
But don't allow the cursor to move bellow the last prompt line."
  :type line
  ;; This successfully prevents the `j' button from moving to an empty line
  ;; bellow the last prompt. However, it still can be bugged for example by
  ;; going to the one line above the last prompt and doing `10j'.
  (when (> (count-words (point) (point-max)) 0)
    (evil-next-line count)))



;; (evil-define-motion vterm-evil-forward-char (count &optional crosslines noerror)
;;   "Move cursor to the right by COUNT characters.
;; TODO"
;;   :type exclusive
;;   (let ((at-last-prompt-line (vterm--at-last-prompt-line-p))
;;         (at-prompt (not (vterm-cursor-in-command-buffer-p (point)))))
;;     (if (and at-last-prompt-line (not at-prompt))
;;         (vterm-send-key "<right>")
;;       (evil-forward-char count crosslines noerror))))


;; (evil-define-motion vterm-evil-backward-char (count &optional crosslines noerror)
;;   "Move cursor to the left by COUNT characters.
;; TODO"
;;   :type exclusive
;;   (let ((at-last-prompt-line (vterm--at-last-prompt-line-p))
;;         (at-prompt (not (vterm-cursor-in-command-buffer-p (- (point) 1)))))
;;     (if (and at-last-prompt-line (not at-prompt))
;;         (vterm-send-key "<left>")
;;       (evil-backward-char count crosslines noerror))))


(evil-define-motion vterm-evil-forward-char (count &optional crosslines noerror)
  "Move cursor to the right by COUNT characters, bypassing line wraps."
  :type exclusive
  (if (get-text-property (1+ (point)) 'vterm-line-wrap)
      (forward-char 2)
    (evil-forward-char count crosslines noerror)))


(evil-define-motion vterm-evil-backward-char (count &optional crosslines noerror)
  "Move cursor to the left by COUNT characters, bypassing line wraps."
  :type exclusive
  (if (and (not (bobp)) (get-text-property (1- (point)) 'vterm-line-wrap))
      (forward-char -1)
    (evil-backward-char count crosslines noerror)))





(general-nmap
  :keymaps 'vterm-mode-map
  "M-:" #'eval-expression
  "j" #'vterm-evil-next-line
  "u" #'frostyx/vterm-undo-redo-message
  "C-r" #'frostyx/vterm-undo-redo-message)

(general-nmap
  :keymaps 'vterm-mode-map
  :states '(normal visual)
  "l" #'vterm-evil-forward-char
  "h" #'vterm-evil-backward-char
  ;; "h" #'evil-collection-vterm-backward-char
  "$" #'vterm-end-of-line
  "^" #'evil-collection-vterm-first-non-blank)

(defun frostyx/vterm-undo-redo-message ()
  "See https://github.com/akermu/emacs-libvterm/issues/592"
  (interactive)
  (message "Undo and redo is too unreliable in Vterm"))


(defun vterm-mouse-set-point (event &optional promote-to-region)
  (interactive "e\np")
  (let ((pt (mouse-set-point event promote-to-region)))
    (if (= (count-words pt (point-max)) 0)
        (vterm-reset-cursor-point)
      pt))
  ;; Otherwise it selects text for every other click
  (keyboard-quit))



(general-define-key
  :keymaps 'vterm-mode-map
  [mouse-1] 'vterm-mouse-set-point)
