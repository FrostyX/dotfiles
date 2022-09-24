;; https://github.com/syl20bnr/spacemacs/issues/12839
(setq package-check-signature nil)

;; load package manager, add the Melpa package registry
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

;; Use latest Org
(use-package org
  :ensure t)

(use-package org-contrib
  :ensure t)

;; Tangle configuration
(org-babel-load-file (expand-file-name "frostyx.org" user-emacs-directory))
