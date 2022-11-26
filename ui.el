(use-package diminish
  :straight t)

(use-package gruvbox-theme
  :straight t
  :config
  (load-theme 'gruvbox-dark-soft t))

(global-set-key (kbd "C-c w w") 'window-swap-states)
