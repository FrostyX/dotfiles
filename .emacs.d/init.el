;; https://github.com/syl20bnr/spacemacs/issues/12839
(setq package-check-signature nil)

;; load package manager, add the Melpa package registry
(require 'package)
(setq package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Install Emacs packages from Nix so that we can have a lockfile and
;; faster onboarding on new machines.
(let ((nix-pkgs-dir (expand-file-name "~/.nix-profile/share/emacs/site-lisp/elpa")))
  (when (file-directory-p nix-pkgs-dir)
    (add-to-list 'package-directory-list nix-pkgs-dir)))

(setq package-enable-at-startup nil)
(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Use latest Org
(use-package org)
(use-package org-contrib)

;; Tangle configuration
(org-babel-load-file (expand-file-name "frostyx.org" user-emacs-directory))
