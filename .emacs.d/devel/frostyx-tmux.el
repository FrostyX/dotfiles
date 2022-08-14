(defun frostyx/tmux-sessions ()
  "List all Tmux sessions"
  (interactive)
  (let* ((choices (emamux:get-sessions)))
	(completing-read "Sessions: " choices)))

(defun frostyx/tmux-windows ()
  "List all windows inside of a specific Tmux session"
  (interactive)
  (let* ((emamux:session "51")
		 (choices (emamux:list-windows)))
	(completing-read "Window: " choices)))

(defun frostyx/tmux-rename-window ()
  "Rename window"
  (interactive)
  (let* ((emamux:session "2")
		 (window (emamux:get-window))
		 (name (read-string "Name: ")))
	(emamux:tmux-run-command t "rename-window" name)))

(defun frostyx/tmux-rename-session ()
  "Rename session"
  (interactive)
  (let* ((emamux:session "2")
		 (name (read-string "Name: ")))
	(emamux:tmux-run-command t "rename-session" "-t" emamux:session name)))

(defun frostyx/hide-status-bar ()
  "Hide status bar"
  (interactive)
  (let* ((emamux:session "9"))
	(emamux:tmux-run-command t "set" "-t" emamux:session "status" "off")))
