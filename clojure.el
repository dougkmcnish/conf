(use-package clojure-mode
  :straight t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :straight t)

(use-package paredit
  :straight t
  :hook (clojure-mode emacs-lisp-mode))




