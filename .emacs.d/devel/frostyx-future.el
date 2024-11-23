;; This file is intentionally not included in my main configuration file. It
;; contains unfinished, half-broken code which would probably make Emacs
;; unusable. Once stabilized, pieces from this file are moved to the main config.


;; I am thinking about using something like this instead of my Hydra setup.

(general-create-definer frostyx/SPC
  :keymaps '(normal visual)
  :prefix "SPC")

(general-create-definer leader-eyebrowse
  :keymaps '(normal visual)
  :prefix "SPC E")

(general-create-definer leader-window
  :keymaps '(normal visual)
  :prefix "SPC W")

(leader-window
  "" '(nil :which-key "Window management")
  "s" '(ace-swap-window :which-key "Swap window"))

(leader-eyebrowse
  "" '(nil :which-key "Eyebrowse")
  "e" 'eyebrowse-switch-to-window-config
  "c" 'eyebrowse-create-named-window-config
  "l" 'eyebrowse-last-window-config)

(frostyx/SPC
  :prefix "SPC D"
  :keymaps '(normal visual)
  ""  '(nil :which-key "Dired")
  "." '(dired-jump :which-key "current directory")
  "~" (list (lambda () (interactive) (dired "~/")) :which-key "home directory")
  "/" (list (lambda () (interactive) (dired "/")) :which-key "root directory")
  "p" (list (lambda () (interactive) (dired (projectile-project-root))) :which-key "project directory"))

(frostyx/SPC
  :prefix "SPC a"
  :keymaps '(normal visual)
  ""  '(nil :which-key "Applications")
  "a" (list (lambda () (interactive) (org-agenda nil "f")) :which-key "Org agenda")
  "r" '(elfeed :which-key "RSS (elfeed)")
  "t" '(frostyx/multi-vterm-named :which-key "Terminal (vterm)")
  "w" '(eww :which-key "web (eww)")
  "e" '(mu4e :which-key "email (mu4e)")
  "s" '(hydra-spotify/body :which-key "Spotify"))
