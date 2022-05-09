(use-package diminish
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-soft t))

(global-set-key (kbd "C-c w w") 'window-swap-states)
