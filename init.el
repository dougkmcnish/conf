(require 'package)
;; package sources for installing org-roam stuff.
;;(set-face-attribute 'default nil :height 160)

(setq inhibit-startup-screen t)
(tool-bar-mode -1)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  )


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
(load "~/.emacs-git.d/yasnippet.el")

(load "~/.emacs-git.d/magit.el")

(load "~/.emacs-git.d/ivy.el")

;;(load "~/.emacs-git.d/clojure.el")

(load "~/.emacs-git.d/markdown-mode.el")
(load "~/.emacs-git.d/org.el")
(load "~/.emacs-git.d/deft.el")
(load "~/.emacs-git.d/org-roam.el")
(load "~/.emacs-git.d/notmuch.el")

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)


