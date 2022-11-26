(use-package deft
  :straight t
  :bind ("<f8>" . deft)
  :commands (deft)
  :config (setq deft-directory "~/.deft"
		deft-default-extension "md"
                deft-extensions '("md" "org")))
