(use-package magit
  :ensure t
  :bind (("C-c g" . magit-file-dispatch))
  :config
  (setq magit-define-global-key-bindings nil))
