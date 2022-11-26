(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ))

(use-package ivy
  :diminish
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind (("C-s" . swiper))
  :config (ivy-mode 1))

(use-package swiper
  :straight t)
