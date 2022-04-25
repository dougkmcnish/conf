(require 'package)
;; package sources for installing org-roam stuff.
;;(set-face-attribute 'default nil :height 160)

(setq inhibit-startup-screen t)
(tool-bar-mode -1)


(defun my/org-time-stamp-now ()
  (interactive)
  (org-time-stamp '(16) t)
  )

(add-to-list 'exec-path "~/.local/bin/")
(setq backup-directory-alist `(("." . "~/.emacs.d/saves/")))

(add-to-list 'package-archives
	     '("melpa" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;;(setq use-package-always-ensure t)

(load "~/.emacs-git.d/ui.el")

(load "~/.emacs-git.d/magit.el")

(load "~/.emacs-git.d/ivy.el")

;;(load "~/.emacs-git.d/clojure.el")

(load "~/.emacs-git.d/org.el")

;; (load "~/.emacs-git.d/org-roam.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(counsel diminish magit ivy use-package monokai-theme gruvbox-theme zenburn-theme solarized-theme muse org-journal)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
