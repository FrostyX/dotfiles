;; https://github.com/syl20bnr/spacemacs/issues/12839
(setq package-check-signature nil)

;; load package manager, add the Melpa package registry
(require 'package)
(setq package-archives
   '(("org" . "https://orgmode.org/elpa/")
    ("melpa" . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; automatically update emacs packages
;; https://emacs.stackexchange.com/a/31904
(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 1
         auto-package-update-prompt-before-update t
         auto-package-update-hide-results t)
   (auto-package-update-at-time "03:00")
   (auto-package-update-maybe))

;; load evil
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)


  ;; https://github.com/emacs-evil/evil-collection
  (setq evil-want-keybinding nil)

  (setq evil-ex-set-initial-state 'normal)

  :config ;; tweak evil after loading it
  (evil-mode)

  ;; example how to map a command in normal mode (called 'normal state' in evil)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit)

  ;; https://www.reddit.com/r/emacs/comments/7dsm0j/how_to_get_evilmode_hjkl_to_work_inside_mx/
  (evil-define-key 'normal package-menu-mode-map (kbd "m") #'package-menu-mark-install)
  (evil-define-key 'normal package-menu-mode-map (kbd "u") #'package-menu-mark-unmark)
  (evil-define-key 'normal package-menu-mode-map (kbd "x") #'package-menu-execute)

  (define-key evil-normal-state-map (kbd ";") 'helm-projectile-switch-to-buffer)
  (define-key evil-normal-state-map (kbd ",;") 'helm-buffers-list)


  ; (define-key evil-normal-state-map (kbd ", f") 'helm-projectile-find-file)
  (define-key evil-normal-state-map (kbd ", f") 'helm-projectile)
  (define-key evil-normal-state-map (kbd ", p") 'helm-projectile-switch-project)

  ;; ctrl+w hjkl is too slow
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  ;; @TODO bind this only in python mode
  (define-key evil-insert-state-map (kbd ",i")
    (lambda ()
      (interactive)
      (insert "import ipdb; ipdb.set_trace()")))




  (defun toggle-fullscreen ()
    ;; https://www.reddit.com/r/emacs/comments/86iq3w/evil_cw_o_toggle_windows/
    (interactive)
    (if (window-parent)
        (delete-other-windows)
      (winner-undo)))

  (define-key evil-normal-state-map (kbd "C-w o") 'toggle-fullscreen)

  ;; (define-key global-map (kbd "C-w o") 'toggle-fullscreen)
  ;; (evil-define-key 'normal evil-normal-state-map (kbd "C-w o") 'toggle-fullscreen)


  ; (evil-define-key* 'motion elfeed-show-mode-map
        ;                               "gb" #'elfeed-show-visit
        ;                               "gj" #'elfeed-show-next
        ;                               "gk" #'elfeed-show-prev)


  (define-key evil-normal-state-map (kbd ", def") 'evil-jump-to-tag)

  (define-key evil-normal-state-map (kbd "C-n") 'neotree-find)
  (evil-define-key 'normal neotree-mode-map (kbd "C-n") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)


  ;; Magit
  (define-key evil-normal-state-map (kbd ",ga") 'magit-stage-file)

  ;; Maybe magit-commit-create
  (define-key evil-normal-state-map (kbd ",gc") 'magit-commit)

  ;; @TODO still asks for something, use more specific function
  (define-key evil-normal-state-map (kbd ",gp") 'magit-push-current)




  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode))

  ; (use-package evil-surround
  ;   :ensure t
  ;   :config
  ;   (global-evil-surround-mode))

  ;; ;; @FIXME missing config
  ;; (use-package evil-indent-textobject
  ;;   :ensure t)

  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode)
    ;; (evil-define-key 'normal evil-commentary-mode-map ", c" 'evil-commentary-line)

    ;; This bind doesn't work for some reason, use `gc` instead
    ;(evil-define-key 'visual evil-commentary-mode-map ", c" 'evil-commentary-line)

    ))


;; ;; https://emacs.stackexchange.com/questions/10350/how-can-i-add-a-new-colon-command-to-evil
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "Gbrowse" 'browse-at-remote))


(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; use evil mode in the buffer created from calling `M-x list-packages'.
;; https://blog.aaronbieber.com/2016/01/23/living-in-evil.html#adding-hjkl-bindings-
(evil-add-hjkl-bindings occur-mode-map 'emacs
  (kbd "/")       'evil-search-forward
  (kbd "n")       'evil-search-next
  (kbd "N")       'evil-search-previous
  (kbd "C-d")     'evil-scroll-down
  (kbd "C-u")     'evil-scroll-up
  (kbd "C-w C-w") 'other-window)

;; Always open help buffers in the current window
;; e.g. opening a package info from `M-x list packages' uses a help mode
;; https://emacs.stackexchange.com/a/22502
(add-to-list 'display-buffer-alist
             '("*Help*" display-buffer-same-window))

;; Close the package detail and go back to the package list by pressing `q'


(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-global-mode)
  (dtrt-indent-adapt))


(use-package neotree
  :ensure t
  :config
  (setq-default neo-show-hidden-files t)
  nil)
;(global-set-key (kbd "C-m") 'neotree-find)


(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-chalk t))


(use-package fic-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fic-mode))


(use-package rainbow-mode
  ;; There is a bug visualizing even #def in .Xdefaults
  :ensure t
  :config
  nil)


(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))


;; (use-package browse-at-remote
;;   ;; https://github.com/rmuslimov/browse-at-remote
;;   :ensure t
;;   :config
;;   nil)

(add-to-list 'load-path "/home/jkadlcik/git/browse-at-remote")
(require 'browse-at-remote)
(setq browse-at-remote-add-line-number-if-no-region-selected nil)


(use-package enwc
  :ensure t
  :config
  (setq enwc-default-backend 'nm)
  (setq enwc-wireless-device 'wlp2s0)
  ;; (define-key enwc-mode-map (kbd "C") 'enwc-connect-to-network-essid)
  nil)

;; (evil-add-hjkl-bindings enwc-mode-map 'emacs
;;   (kbd "/")       'evil-search-forward
;;   (kbd "n")       'evil-search-next
;;   (kbd "N")       'evil-search-previous
;;   (kbd "C-d")     'evil-scroll-down
;;   (kbd "C-u")     'evil-scroll-up
;;   (kbd "C-w C-w") 'other-window)

;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-default-theme))


(use-package projectile
  :ensure t
  :config
;; @TODO create key binding for `projectile-discover-projects-in-search-path'
  (setq projectile-project-search-path '("~/git/")))



(use-package spaceline
  :ensure t
  :config

  ;; http://chriskempson.com/projects/base16/
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (set-face-attribute 'spaceline-evil-normal nil :background (plist-get base16-chalk-colors :base01) :foreground (plist-get base16-chalk-colors :base09))
  (set-face-attribute 'spaceline-evil-emacs nil :background (plist-get base16-chalk-colors :base01) :foreground (plist-get base16-chalk-colors :base0D))
  (set-face-attribute 'spaceline-evil-insert nil :background (plist-get base16-chalk-colors :base01) :foreground (plist-get base16-chalk-colors :base0B))
  (set-face-attribute 'spaceline-evil-replace nil :background (plist-get base16-chalk-colors :base01) :foreground (plist-get base16-chalk-colors :base0D))
  (set-face-attribute 'spaceline-evil-motion nil :background (plist-get base16-chalk-colors :base01) :foreground (plist-get base16-chalk-colors :base0D))
  (set-face-attribute 'spaceline-evil-visual nil :background (plist-get base16-chalk-colors :base01) :foreground (plist-get base16-chalk-colors :base0E))

  (spaceline-spacemacs-theme))



;; http://chriskempson.com/projects/base16/
(set-face-attribute 'lazy-highlight nil :background (plist-get base16-chalk-colors :base09)
                                        :foreground (plist-get base16-chalk-colors :base01))


(use-package dashboard
  :ensure t
  ; :init
  ; (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  ; (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
  :config
  (setq dashboard-startup-banner 'logo)

  (setq dashboard-items '((projects . 5)
                          (recents  . 5)
                          (custom . t)
                          (agenda . 5)))

  ;; We can't have startup hook for dashboard, because we
  ;; have startup hook to restore previous buffers and frames
  ;; @TODO dashboard would still be usefull, I just need keybinding for dashboard-setup-startup-hook
  ;; (it))
  )


(defun dashboard-insert-custom (list-size)
  (dashboard-insert-section
   "Applications"
   '("elfeed"
     "spotify-my-playlists"
     "spotify-recently-played"
     "mu4e")
     "eww"
   list-size
   "a"
   `(lambda (&rest ignore) (command-execute (intern ,el)))
   (format "%s" el)))

(add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))


(use-package password-store
  :ensure t
  :config
  nil)




;; EWW notes here
;; Key bindings
;; https://github.com/emacs-evil/evil-collection/blob/master/modes/eww/evil-collection-eww.el
;; o - open
;; go - open in external browser



; (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
; (add-to-list 'dashboard-item-generators  '(custom . describe-package-widget))




; (add-to-list 'dashboard-items '(custom) t)


; (use-package swiper
;       :ensure t
;     :config
;     (ivy-mode 1)
;     ; (global-set-key (kbd "M-x") 'counsel-M-x)
; )



;; This spotify library is not in MELPA yet,
;; see https://github.com/danielfm/spotify.el/issues/44
;;
;; Also, for playing, web player (or other device) must be opened
;; See https://github.com/danielfm/spotify.el/#selecting-a-device-for-playback
(add-to-list 'load-path "/home/jkadlcik/git/spotify.el")
(require 'spotify)
(setq spotify-oauth2-client-id (password-store-get "spotify/client-id"))
(setq spotify-oauth2-client-secret (password-store-get "spotify/client-secret"))
(setq spotify-transport 'connect)
(define-key spotify-mode-map (kbd "C-c .") 'spotify-command-map)

;; https://github.com/danielfm/spotify.el#remote-minor-mode
(evil-define-key 'normal spotify-mode-map (kbd "q") 'spotify-quit)  ;; @FIXME doesn't work, probably create an issue
(evil-define-key 'normal spotify-mode-map (kbd "go") 'spotify-track-select)
(evil-define-key 'normal spotify-mode-map (kbd "gp") 'spotify-toggle-play)
(evil-define-key 'normal spotify-mode-map (kbd "g<") 'spotify-previous-track)
(evil-define-key 'normal spotify-mode-map (kbd "g>") 'spotify-next-track)
(evil-define-key 'normal spotify-mode-map (kbd "gd") 'spotify-select-device)





(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (setq helm-locate-fuzzy-match t)


  (global-set-key (kbd "M-x") #'helm-M-x)
  ; (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  ; (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  ; http://cachestocaches.com/2016/12/vim-within-emacs-anecdotal-guide/
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-h") 'helm-next-source)

  (define-key helm-map [escape] 'helm-keyboard-quit)

  ; (define-key helm-map (kbd "C-S-h") 'describe-key)
  ; (define-key helm-map (kbd "C-l") (kbd "RET"))
  ; (dolist (keymap (list helm-find-files-map helm-read-file-map))
        ; (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
        ; (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
        ; (define-key keymap (kbd "C-S-h") 'describe-key))


  (use-package helm-posframe
    :ensure t
    :config
    (helm-posframe-enable)
    (setq helm-posframe-poshandler
          ;; #'posframe-poshandler-frame-top-center)
          #'posframe-poshandler-frame-center)
    (setq helm-posframe-width 200)
    (setq helm-posframe-height 600)
    (setq helm-posframe-parameters
          '((left-fringe . 10)
            (right-fringe . 10))))
)



;; Create my own spacemacs-like menu
(use-package hydra
  :ensure t
  :bind
  ("C-SPC" . hydra-main/body)
  :config
  (define-key evil-normal-state-map (kbd "SPC") 'hydra-main/body)
  :custom
  (hydra-default-hint nil))


(defhydra hydra-main (:color blue)
  "
    ^
    ^Hydra^              ^Misc^
    ^^^^───────────────────────
    _q_ quit            _a_ applications
    ^^                  _z_ zoom
  "
  ("SPC" nil)
  ("q" nil)
  ("a" hydra-applications/body)
  ("z" hydra-zoom/body))


(defhydra hydra-zoom (:color blue)
  "
    ^
    ^Zoom^              ^Actions^
    ^^^^─────────────────────────
    _q_ quit            _l_ lesser
    ^^                  _g_ greater
  "
  ("SPC" nil)
  ("q" nil)
  ("l" text-scale-decrease)
  ("g" text-scale-increase))


(defhydra hydra-applications (:color blue)
  "
    ^
    ^Applications^              ^Launch^
    ^^^^────────────────────────────────
    _q_ quit            _r_ RSS (elfeed)
    ^^                  _w_ web (eww)
    ^^                  _e_ email (mu4e)
    ^^                  _p_ spotify - my playlists
    ^^                  _s_ spotify - recently played
  "
  ("SPC" nil)
  ("q" nil)
  ("r" elfeed)
  ("w" eww)
  ("e" mu4e)
  ("p" spotify-my-playlists)
  ("s" spotify-recently-played))


(use-package eyebrowse
  ;; https://github.com/wasamasa/eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t)
  (eyebrowse-setup-opinionated-keys)

  :bind
    (:map evil-normal-state-map
       (", e" . 'eyebrowse-switch-to-window-config)
       (", c" . 'eyebrowse-create-window-config)
       (", $" . 'eyebrowse-rename-window-config)

       ;; unmap `gc' because it conflicts with `evil-commentary'
       ("g c" . nil)
  
     :map evil-motion-state-map
       ("g c" . nil)

))


;; Saving sessions

;; (setq desktop-auto-save-timeout nil
;;       desktop-save 'ask-if-new
;;       desktop-path (list desktop-dirname)
;;       desktop-load-locked-desktop nil)


;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(desktop-save-mode 1)
;; (add-to-list 'window-persistent-parameters '(window-side . writable))
;; (add-to-list 'window-persistent-parameters '(window-slot . writable))

(setq desktop-restore-forces-onscreen nil)
(add-hook 'desktop-after-read-hook
 (lambda ()
   (frameset-restore
    desktop-saved-frameset
    :reuse-frames (eq desktop-restore-reuses-frames t)
    :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
    :force-display desktop-restore-in-current-display
    :force-onscreen desktop-restore-forces-onscreen)))


;; (use-package perspective
;;   :ensure t
;;   :config

;;   (setq persp-state-default-file "/home/jkadlcik/.emacs.d/persp-save-default")
;;   (setq persp-disable-buffer-restriction-once t)

;;   ;; (use-package persp-projectile 
;;   ;;   :ensure t
;;   ;;   :config
;;   ;;   nil)

;;   (persp-mode t))


;; (use-package centaur-tabs
;;   ;; https://github.com/ema2159/centaur-tabs
;;   ;; :ensure t
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)

;;   (centaur-tabs-headline-match)
;;   (setq centaur-tabs-style "box")
;;   (setq centaur-tabs-height 32)
;;   (setq centaur-tabs-set-icons t)
;;   (setq centaur-tabs-gray-out-icons 'buffer)

;;   (setq centaur-tabs-set-bar 'under)
;;   (setq x-underline-at-descent-line t)

;;   ;; (setq centaur-tabs--buffer-show-groups t)

;;   (centaur-tabs-group-by-projectile-project)

;;   :bind
;;   (:map evil-normal-state-map
;;     ("g t" . centaur-tabs-forward)
;;     ("g T" . centaur-tabs-backward))
;;  )





; (use-package company-jedi             ;;; company-mode completion back-end for Python JEDI
;   :ensure t
;   :config
;   (setq jedi:environment-virtualenv
;       (list (expand-file-name "~/.emacs.d/.python-environments/")))
;   (setq jedi:environment-virtualenv
;       (append python-environment-virtualenv
;                       '("--python" "/usr/bin/python3")))
;   (add-hook 'python-mode-hook 'jedi:setup)
;   (setq jedi:complete-on-dot t)
;   (setq jedi:use-shortcuts t)
;   (defun config/enable-company-jedi ()
;     (add-to-list 'company-backends 'company-jedi))
;   (add-hook 'python-mode-hook 'config/enable-company-jedi))




;; When messing with the configuration and going back to elpy, delete
;; these packages with `M-x package-delete'
;;
;; (use-package company
;;   :ensure t
;;   :config
;;   (global-company-mode))

;; (use-package flycheck
;;   :ensure t)

;; (use-package lsp-mode
;;   :ensure t
;;   ;; Support for specific languages:
;;   ;; Python: pip-3 install --user python-language-server[all]

;;   :init
;;   (setq lsp-keymap-prefix "s-l")

;;   ;;@FIXME autocompletion inserts function arguments and it sux
;;   ;;       using elpy until that
;;   ;; :hook
;;   ;; ((python-mode . lsp))

;;   :config
;;   (setq lsp-enable-symbol-highlighting nil)
;;   (setq lsp-signature-auto-activate nil)
;;   (setq lsp-enable-snippet nil)
;;   (setq lsp-enable-completion-at-point nil)

;;   :commands lsp)

;; (use-package lsp-ui
;;   :disabled
;;   :ensure t
;;   :commands lsp-ui-mode)

;; (use-package company-lsp
;;   :ensure t
;;   :config
;;   (setq company-lsp-enable-snippet nil)
;;   :commands company-lsp)

;; (use-package helm-lsp
;;   :ensure t
;;   :commands helm-lsp-workspace-symbol)

;; (setq company-minimum-prefix-length 1
;;       company-idle-delay 0) ;; default is 0.2

;; (add-hook 'prog-mode-hook (lambda () (highlight-indentation-mode -1)))


(use-package elpy
  ;; sudo dnf install python3-virtualenv
  :ensure t
  :init
  (setq elpy-modules
        '(elpy-module-sane-defaults
          elpy-module-company
          elpy-module-eldoc
          elpy-module-django))
  (elpy-enable))



;; disable UI nonsense
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(save-place-mode 1)

; (setq base16-distinct-fringe-background nil)
; (set-face-attribute 'line-number-current-line nil
;                     :background "grey"
;                                       :foreground nil)

; (set-face-attribute 'line-number-current-line nil
;                     :inverse-video f)


;; https://emacsredux.com/blog/2014/08/27/a-peek-at-emacs-24-dot-4-superword-mode/
;; #@FIXME using this manually in a python file works, but this config setting not
;; Probably `global-*' mode settings will be required here. Wihout global, the setting
;; is only for the current buffer
;; (superword-mode)
(global-superword-mode)


;; https://emacs.stackexchange.com/a/9584
;; Superword mode seems to apply only on searching, adding also this,
;; to get w, yiw, dw, etc working as expected
(modify-syntax-entry ?_ "w")
;; (defadvice evil-inner-word (around underscore-as-word activate)
;;   (let ((table (copy-syntax-table (syntax-table))))
;;     (modify-syntax-entry ?_ "w" table)
;;     (with-syntax-table table
;;       ad-do-it)))


;; https://www.emacswiki.org/emacs/AutoPairs
(electric-pair-mode)


;; Middle-click paste where cursor is, don't care about mouse position
;; https://superuser.com/questions/330849/can-i-tell-emacs-to-paste-middle-mouse-button-on-the-cursor-position
(setq mouse-yank-at-point t)


;; Automatically reload changed files
;; https://stackoverflow.com/q/1480572/3285282
(global-auto-revert-mode t)


;; keyboard scroll one line at a time
;; scroll when cursor is five lines from the edge
;; and don't ever recenter the cursor when scrolling
(setq scroll-step 1)
(setq scroll-margin 5)
(setq scroll-conservatively 101)


;; Don't mess CWD with #foo.py# and foo.py~ files
;; https://emacs.stackexchange.com/a/34
;; (setq backup-directory-alist '(("." . "~/MyEmacsBackups")))
;; (setq backup-directory-alist '("~/MyEmacsBackups"))
;; (setq backup-directory-alist `(("." . "~/MyEmacsBackups")))


;; https://stackoverflow.com/a/18330742/3285282
(defvar --backup-directory (concat user-emacs-directory "MyEmacsBackups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq backup-directory-alist `((,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )



;; @TODO try this after restarting emacs
;; https://stackoverflow.com/questions/15946178/change-the-color-of-the-characters-in-whitespace-mode
;; (setq whitespace-display-mappings
;;   ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
;;   '(
;;     (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
;;     (newline-mark 10 [182 10]) ; 10 LINE FEED
;;     (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
;;     ))



;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (set-window-fringes (selected-window) 0 0 nil)))


;; (add-hook 'writeroom-mode-hook
;; 	  (lambda ()
;;             (set-window-fringes (selected-window) 0 0 nil)))


(setq writeroom-fullscreen-effect (quote maximized))
(add-hook 'writeroom-mode-hook
        (lambda ()
          (if writeroom-mode
              (progn
                (blink-cursor-mode 0)  ; turn off blinky cursor
                (fringe-mode 0)  ; hide fringes
                (setq-local inhibit-message t))  ; don't distract
            (progn
              (blink-cursor-mode t)
              (fringe-mode nil)
              (setq-local inhibit-message t)))))


(set-face-attribute 'line-number-current-line nil
  :inverse-video nil
  ;https://github.com/belak/base16-emacs/commit/f701a8e191ae9c0bd6ab93926ce993bb18a9e98c
  ; :foreground "base03"
  ; :background "base01")
)

; (set-face-attribute 'line-number-current-line 'line-number)


(set-face-attribute
  'default nil
  :family "vera sans"
  :height 90
  :weight 'normal
  :width 'normal)


;; (set-face-attribute 'default nil :font "Noto Sans Mono")


(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

(setq evil-default-state 'normal) ;; changes default state to emacs


; (global-set-key (kbd ";") 'helm-buffers-list)




;; @TODO
;; - or example, I created a <leader>x mapping for M-x)
;; - Namapovat :e na :edit ... :e nefunguje by default


;; @TODO use the all-the-icons package and execute all-the-icons-install-fonts


;; @TODO use jinja2-mode package



(use-package elfeed
  :ensure t
  :commands (elfeed)


  :commands elfeed
        ; :bind (:map elfeed-search-mode-map
        ;          ("<SPC>" . next-line)
        ;          ("U" . elfeed-unjam)
        ;          :map elfeed-show-mode-map
        ;          ("S-<SPC>" . scroll-down-command))

  :config
  (setq-default elfeed-search-filter "")
  (setq elfeed-feeds
  '("http://nullprogram.com/feed/"
    "http://planet.emacsen.org/atom.xml")))



(with-eval-after-load "elfeed-search"
  (evil-define-key* 'motion elfeed-search-mode-map
                                        "RET" #'elfeed-search-show-entry
                                        "go" #'elfeed-search-show-entry
                                        "gb" #'elfeed-search-browse-url
                                        "gr" #'elfeed-search-update--force
                                        "gR" #'elfeed-search-fetch))

(with-eval-after-load "elfeed-show"
(evil-define-key* 'motion elfeed-show-mode-map
                  "gb" #'elfeed-show-visit
                  "gj" #'elfeed-show-next
                  "gk" #'elfeed-show-prev)
)

; (add-to-list 'evil-motion-state-modes 'elfeed-search-mode)
; (add-to-list 'evil-motion-state-modes 'elfeed-show-mode)

; (with-eval-after-load 'evil
;     (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode))
    ; (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode))


;; Jump to next word with `w' should jump
;; CMD ["runuser", "-u", "foo", "-g", "bar", "/usr/bin/importer_runner.py"]
;; https://github.com/syl20bnr/spacemacs/issues/9740
;; (with-eval-after-load 'evil
;;   (defalias #'forward-evil-word #'forward-evil-symbol))





;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay      0.5
;;           treemacs-directory-name-transformer    #'identity
;;           treemacs-display-in-side-window        t
;;           treemacs-eldoc-display                 t
;;           treemacs-file-event-delay              5000
;;           treemacs-file-extension-regex          treemacs-last-period-regex-value
;;           treemacs-file-follow-delay             0.2
;;           treemacs-file-name-transformer         #'identity
;;           treemacs-follow-after-init             t
;;           treemacs-git-command-pipe              ""
;;           treemacs-goto-tag-strategy             'refetch-index
;;           treemacs-indentation                   2
;;           treemacs-indentation-string            " "
;;           treemacs-is-never-other-window         nil
;;           treemacs-max-git-entries               5000
;;           treemacs-missing-project-action        'ask
;;           treemacs-no-png-images                 nil
;;           treemacs-no-delete-other-windows       t
;;           treemacs-project-follow-cleanup        nil
;;           treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                      'left
;;           treemacs-recenter-distance             0.1
;;           treemacs-recenter-after-file-follow    nil
;;           treemacs-recenter-after-tag-follow     nil
;;           treemacs-recenter-after-project-jump   'always
;;           treemacs-recenter-after-project-expand 'on-distance
;;           treemacs-show-cursor                   nil
;;           treemacs-show-hidden-files             t
;;           treemacs-silent-filewatch              nil
;;           treemacs-silent-refresh                nil
;;           treemacs-sorting                       'alphabetic-asc
;;           treemacs-space-between-root-nodes      t
;;           treemacs-tag-follow-cleanup            t
;;           treemacs-tag-follow-delay              1.5
;;           treemacs-width                         35)

;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     ;;(treemacs-resize-icons 44)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode t)
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple))))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after treemacs evil
;;   :ensure t)

; (use-package treemacs-projectile
;   :after treemacs projectile
;   :ensure t)

; (use-package treemacs-icons-dired
;   :after treemacs dired
;   :ensure t
;   :config (treemacs-icons-dired-mode))

; (use-package treemacs-magit
;   :after treemacs magit
;   :ensure t)


; (use-package company
;   :ensure t
;   :config
;   (company-mode t)
;
; )



;; (use-package gnus
;;   :ensure t
;;   :config
;;   nil)


;; ;;;; RECEIVE
;; ;; (setq gnus-secondary-select-methods
;; ;;       '((nnimap "gmail"
;; ;;                 (nnimap-address "imap.gmail.com")
;; ;;                 (nnimap-server-port 993)
;; ;;                 (nnimap-authenticator login)
;; ;;                 (nnimap-expunge-on-close 'never)
;; ;;                 (nnimap-stream ssl))))


;; (setq user-mail-address "jakub.kadlcik@gmail.com"
;;       user-full-name "Jakub Kadlčík")

;; ;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; (setq gnus-select-method
;;       '(nnimap "gmail"
;;                (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.                                        
;;                (nnimap-server-port 993)
;;                (nnimap-stream ssl)))


;; (use-package notmuch
;;   ;; requires `dnf install notmuch'
;;   :ensure t
;;   :config

;;   (setq user-full-name "Jakub Kadlčík")
;;   (setq user-mail-adress "frostyx@email.cz")
;;   (setq mail-user-agent 'message-user-agent)
;;   (setq mail-specify-envelope-from t)

;;   (setq notmuch-search-oldest-first nil)


;;   nil)

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;; (add-to-list 'load-path "/home/jkadlcik/git/spotify.el")
(require 'mu4e)


;; sudo dnf install maildir-utils

(use-package mu4e
  :ensure nil
  :config
  (setq mu4e-maildir (expand-file-name "~/Mail/seznam"))

  ;; https://etienne.depar.is/emacs.d/mu4e.html
  (setq mu4e-split-view 'vertical)
  (setq mu4e-headers-visible-columns 67)
  (setq mu4e-html2text-command "w3m -dump -T text/html -cols 72 -o display_link_number=true -o auto_image=false -o display_image=false -o ignore_null_img_alt=true")
  (setq mu4e-headers-date-format "%d. %m. %Y")
  (setq mu4e-headers-time-format "%H:%M")
  (setq mu4e-view-show-addresses t)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-get-mail-command "mbsync -V seznam")

  ;; (setq )
  ;; (setq )

  (setq mu4e-headers-show-threads t)  ;; does this work?

  (setq mu4e-update-interval 120) ;; two minutes
  (setq mu4e-use-fancy-chars t)

  (setq user-full-name "Jakub Kadlčík")
  (setq user-mail-adress "frostyx@email.cz")
  (setq mail-user-agent 'message-user-agent)
  (setq mail-specify-envelope-from t)


  ;; (setq mu4e-headers-unread-mark  '("u" . "🖂"))
  ;; (setq mu4e-headers-attach-mark '("a" . "📎"))

  (setq mu4e-headers-unread-mark '("u" . "u")
        mu4e-headers-draft-mark '("D" . "D") ; draft
        mu4e-headers-seen-mark '("S" . "S") ; seen
        mu4e-headers-unseen-mark '("u" . "u") ; unseen
        mu4e-headers-flagged-mark '("F" . "F") ; flagged
        mu4e-headers-new-mark '("N" . "N") ; new
        mu4e-headers-replied-mark '("R" . "R") ; replied
        mu4e-headers-passed-mark '("P" . "P") ; passed
        mu4e-headers-encrypted-mark '("x" . "x") ; encrypted
        mu4e-headers-signed-mark '("s" . "s")) ; signed

  ;; the headers to show in the headers list -- a pair of a field
  ;; and its width, with `nil' meaning 'unlimited'
  ;; (better only use that for the last field.
  ;; These are the defaults:
  (setq mu4e-headers-fields
      '((:human-date . 20)    ;; alternatively, use :date
        (:flags . 10)
        (:from . 30)
        (:subject . nil))) ;; alternatively, use :thread-subject


  ;; @TODO notifications
  ;; https://github.com/iqbalansari/mu4e-alert

  nil)


(use-package w3m
  :ensure t
  :config
  nil)




;; (use-package mu4e-conversation
;;   :ensure t
;;   :config
;;   (mu4e-conversation-mode))


;; (with-eval-after-load 'mu4e
;;     (cb/mu4e-config-threads))



;; (setq sendmail-program "/usr/local/bin/msmtp"
;; 	  mail-specify-envelope-from t
;; 	  mail-envelope-from 'header
;; 	  message-sendmail-envelope-from 'header)

; (custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  )
;
; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(custom-safe-themes
;    (quote
;     ("e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" default)))
;  '(package-selected-packages (quote (base16-theme evil use-package))))
; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(package-selected-packages (quote (helm use-package evil base16-theme))))
; (custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(electric-pair-mode t)
 '(enwc-wired-device "wlp2s0")
 '(enwc-wireless-device "wlp2s0")
 '(helm-completion-style (quote emacs))
 '(package-selected-packages
   (quote
    (auto-package-update nov elpy enwc password-store w3m persp-projectile perspective eyebrowse centaur-tabs browse-at-remote magit jinja2-mode writeroom-mode markdown-mode docker-compose-mode dockerfile-mode powerline mu4e-conversation mu4e evil-collection notmuch rainbow-delimiters fic-mode neotree dtrt-indent evil-commentary use-package treemacs-icons-dired treemacs-evil transient swiper spotify helm-projectile helm-fuzzy-find git-commit fzf evil-leader evil-indent-textobject elfeed dashboard company-jedi base16-theme)))
 '(pixel-scroll-mode t)
 '(projectile-mode t nil (projectile))
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; See configs from other people
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org
;; https://jamiecollinson.com/blog/my-emacs-config/
;; https://emacs.nasy.moe/
