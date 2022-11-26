(use-package elfeed
  :straight t)


(use-package elfeed-protocol
  :straight t
  :after elfeed
  :bind (("C-c o e" . elfeed))
  :init
  (elfeed-protocol-enable)
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-protocol-newsblur-maxpages 20)
  (setq elfeed-curl-extra-arguments '("--cookie-jar" "/tmp/newsblur-cookie"
                                    "--cookie" "/tmp/newsblur-cookie"))
  (setq shr-max-image-proportion 0.3))

(use-package elfeed-autotag
  :after elfeed
  :straight t)

(use-package elfeed-org
  :straight t
  :after elfeed
  :init
  (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs-git.d/elfeed.org")))
