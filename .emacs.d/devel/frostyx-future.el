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

(set-face-attribute
  'org-block nil
  :foreground my/lwhite)

(set-face-attribute
  'link nil
  :foreground my/blue)

(set-face-attribute
  'lazy-highlight nil
  :background (cdr (assoc 'surface1 catppuccin-mocha-colors)))

(dolist (face '(org-level-1
                org-level-2
                org-level-3
                org-level-4
                org-level-5
                org-level-6
                markdown-header-face-1
                markdown-header-face-2
                markdown-header-face-3
                markdown-header-face-4
                markdown-header-face-5
                markdown-header-face-6))
  (set-face-attribute
   face nil
   :weight 'normal
   :height 90))


(dolist (face '(diff-hunk-header
                magit-section-highlight
                magit-diff-added-highlight
                magit-diff-removed-highlight
                magit-diff-context-highlight))
  (set-face-attribute
   face nil
   :background (cdr (assoc 'mantle catppuccin-mocha-colors))))


(dolist (face '(fringe line-number))
  (set-face-attribute
   face nil
   :background (cdr (assoc 'mantle catppuccin-mocha-colors))))

(set-face-attribute
  'font-lock-variable-name-face nil
  :foreground my/red)

(set-face-attribute
  'mu4e-unread-face nil
  :foreground my/purple)

(set-face-attribute
  'isearch nil
  :weight 'normal
  :background my/yellow)

(set-face-attribute
  'gnus-header-content nil
  :weight 'normal
  :foreground my/lwhite)

(set-face-attribute
  'gnus-header-name nil
  :weight 'normal
  :foreground my/blue)

(set-face-attribute
  'gnus-header-subject nil
  :weight 'normal
  :foreground my/purple)

(set-face-attribute
  'gnus-header-from nil
  :weight 'normal
  :foreground my/purple)

(set-face-attribute
  'gnus-header-content nil
  :weight 'normal
  :foreground my/lwhite)

(set-face-attribute
  'gnus-button nil
  :weight 'normal
  :foreground my/blue)

(set-face-attribute
  'magit-hash nil
  :foreground my/blue)

(set-face-attribute
  'magit-log-author nil
  :foreground my/blue)

(set-face-attribute
  'magit-log-date nil
  :foreground my/lwhite)

(set-face-attribute
  'org-super-agenda-header nil
  :foreground my/purple)

(set-face-attribute
  'org-agenda-structure nil
  :foreground my/purple)

;; (set-face-attribute
;;   'org-tag nil
;;   :foreground my/lgray)

(set-face-attribute
  'company-tooltip-common nil
  :foreground my/blue)

(set-face-attribute
  'company-tooltip-common-selection nil
  :weight 'normal
  :foreground my/blue)

(set-face-attribute
  'company-tooltip-selection nil
  :foreground my/blue
  :background my/black)

(set-face-attribute
  'company-tooltip nil
  :foreground my/lwhite
  :background nil)

(set-face-attribute
  'region nil
  :background (cdr (assoc 'surface0 catppuccin-mocha-colors)))

(set-face-attribute
  'mu4e-header-key-face nil
  :foreground my/blue)

(set-face-attribute
  'mu4e-header-key-face nil
  :foreground my/blue)

(set-face-attribute
  'mu4e-highlight-face nil
  :weight 'normal
  :foreground my/purple)
