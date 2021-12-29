;; https://github.com/syl20bnr/spacemacs/issues/12839
(setq package-check-signature nil)

;; load package manager, add the Melpa package registry
(require 'package)
(setq package-archives
   '(("melpa" . "https://melpa.org/packages/")
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
 '(ansi-term-color-vector
   [unspecified "#151515" "#fb9fb1" "#acc267" "#ddb26f" "#6fc2ef" "#e1a3ee" "#6fc2ef" "#d0d0d0"] t)
 '(electric-indent-mode nil)
 '(electric-pair-mode t)
 '(enwc-wired-device "wlp2s0")
 '(enwc-wireless-device "wlp2s0")
 '(helm-completion-style 'emacs)
 '(horizontal-scroll-bar-mode nil)
 '(hydra-default-hint nil)
 '(package-selected-packages
   '(helm-spotify-plus helm-spotify helm-ls-git helm-ag evil-magit vterm toggle-quotes rpm-spec-mode yasnippet circe-notifications circe elfeed-org diff-hl adoc-mode flycheck fill-column-indicator yasnippet-snippets major-mode-hydra pulseaudio-control bluetooth elm-mode aggressive-indent toc-org pretty-hydra hydra-posframe auto-package-update nov elpy enwc password-store w3m persp-projectile perspective eyebrowse centaur-tabs browse-at-remote magit jinja2-mode writeroom-mode markdown-mode docker-compose-mode dockerfile-mode powerline mu4e-conversation mu4e evil-collection notmuch rainbow-delimiters fic-mode neotree dtrt-indent evil-commentary use-package treemacs-icons-dired treemacs-evil transient swiper spotify helm-projectile helm-fuzzy-find git-commit fzf evil-leader evil-indent-textobject elfeed company-jedi base16-theme))
 '(pixel-scroll-mode t)
 '(projectile-mode t nil (projectile))
 '(safe-local-variable-values '((encoding . utf-8)))
 '(scroll-bar-mode nil)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markup-big-face ((t (:inherit markup-gen-face :height 1.3))))
 '(markup-list-face ((t (:inherit markup-meta-face :background "black" :foreground "plum1"))))
 '(markup-title-0-face ((t (:inherit markup-gen-face :height 2.0))))
 '(markup-title-1-face ((t (:inherit markup-gen-face :height 1.6))))
 '(markup-title-2-face ((t (:inherit markup-gen-face :height 1.4))))
 '(markup-title-3-face ((t (:inherit markup-gen-face :weight bold :height 1.2))))
 '(markup-verbatim-face ((t (:background "black" :foreground "magenta")))))
(put 'narrow-to-region 'disabled nil)
