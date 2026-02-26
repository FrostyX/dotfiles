;; Tree sitter
;; See https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (gleam "https://github.com/gleam-lang/tree-sitter-gleam")))

;; (setq treesit-language-source-alist
;;       '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;         (cmake "https://github.com/uyha/tree-sitter-cmake")
;;         (css "https://github.com/tree-sitter/tree-sitter-css")
;;         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;         (go "https://github.com/tree-sitter/tree-sitter-go")
;;         (html "https://github.com/tree-sitter/tree-sitter-html")
;;         (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
;;                     "master" "src")
;;         (json "https://github.com/tree-sitter/tree-sitter-json")
;;         (make "https://github.com/alemuller/tree-sitter-make")
;;         (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;         (python "https://github.com/tree-sitter/tree-sitter-python")
;;         (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;         (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
;;              "master" "tsx/src")
;;         (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
;;                     "master" "typescript/src")
;;         (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package clojure-ts-mode
  :ensure t)

(use-package treesit-auto
  :ensure t
  :custom
  (setq treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist '(gleam))
  ;; (treesit-auto-add-to-auto-mode-alist '(python clojure yaml bash css json))
  ;; (treesit-auto-add-to-auto-mode-alist 'all)
  ; (treesit-auto-install-all)
  (global-treesit-auto-mode))

;; (setq major-mode-remap-alist
;;  '((yaml-mode . yaml-ts-mode)
;;    (bash-mode . bash-ts-mode)
;;    (js2-mode . js-ts-mode)
;;    (typescript-mode . typescript-ts-mode)
;;    (json-mode . json-ts-mode)
;;    (css-mode . css-ts-mode)
;;    (python-mode . python-ts-mode)))

(use-package tree-sitter-indent
  :ensure t)

;; (setq major-mode-remap-alist
;;       '((python-mode . python-ts-mode)
;;         (gleam-mode . gleam-ts-mode)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp"))))

(use-package eglot
  :ensure t
  :hook
  ((python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   (markdown-mode . eglot-ensure)
   (elm-mode . eglot-ensure)
   (clojurescript-mode . eglot-ensure)
   (clojure-ts-mode . eglot-ensure)))


(with-eval-after-load 'eglot
  (let ((cmd '("gleam" "lsp")))
    (add-to-list 'eglot-server-programs `(gleam-mode . ,cmd))
    (add-to-list 'eglot-server-programs `(gleam-ts-mode . ,cmd))))
