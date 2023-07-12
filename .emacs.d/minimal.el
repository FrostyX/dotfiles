;; emacs -q --load ~/.emacs.d/minimal.el
;;
;; For demos, you might want to:
;;   `M-x load-theme RET ef-light'
;;   `M-x auto-dim-other-buffers-mode'

;; More ef themes:
;; https://protesilaos.com/emacs/ef-themes-pictures
;; (ef-day, ef-summer)

(require 'package)
(setq package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-enable-at-startup nil)
(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


(setq evil-want-keybinding nil)
(setq evil-want-integration t)

(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-ex-set-initial-state 'normal)
  :config
  (evil-mode))
