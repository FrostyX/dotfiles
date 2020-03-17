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

;; Use latest Org
(use-package org :ensure org-plus-contrib)

;; Tangle configuration
(org-babel-load-file (expand-file-name "frostyx.org" user-emacs-directory))
(garbage-collect)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(electric-pair-mode t)
 '(enwc-wired-device "wlp2s0")
 '(enwc-wireless-device "wlp2s0")
 '(helm-completion-style (quote emacs))
 '(hydra-default-hint nil)
 '(package-selected-packages
   (quote
    (pretty-hydra hydra-posframe auto-package-update nov elpy enwc password-store w3m persp-projectile perspective eyebrowse centaur-tabs browse-at-remote magit jinja2-mode writeroom-mode markdown-mode docker-compose-mode dockerfile-mode powerline mu4e-conversation mu4e evil-collection notmuch rainbow-delimiters fic-mode neotree dtrt-indent evil-commentary use-package treemacs-icons-dired treemacs-evil transient swiper spotify helm-projectile helm-fuzzy-find git-commit fzf evil-leader evil-indent-textobject elfeed company-jedi base16-theme)))
 '(pixel-scroll-mode t)
 '(projectile-mode t nil (projectile))
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
