(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;(require 'package)
; package sources for installing org-roam stuff.
; (set-face-attribute 'default nil :height 160)

(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; UI Cleanup 
(setq inhibit-startup-screen t)
(tool-bar-mode -1)

;; Transparent encryption
(setenv "GPG_AGENT_INFO" nil)
(setq epa-pinentry-mode 'loopback)

(setq auth-sources
      '((:source "~/.authinfo.gpg")))

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  )


(defun my/org-time-stamp-now ()
  (interactive)
  (org-time-stamp '(16) t)
  )

(add-to-list 'exec-path "~/.local/bin/")

(setq backup-directory-alist `(("." . "~/.emacs.d/saves/")))

; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;                         ("melpa" . "https://melpa.org/packages/")
 ;                        ("org" . "http://orgmode.org/elpa/")))


(straight-use-package 'use-package)

; (package-initialize)
; (unless package-archive-contents
;   (package-refresh-contents))

; (unless (package-installed-p 'use-package)
;  (package-install 'use-package))

; (require 'use-package)

; (setq use-package-always-ensure t)

(load "~/.emacs-git.d/ui.el")
(load "~/.emacs-git.d/toolbox-tramp.el")
(load "~/.emacs-git.d/yasnippet.el")
(load "~/.emacs-git.d/mastodon.el")
(load "~/.emacs-git.d/magit.el")
(load "~/.emacs-git.d/ivy.el")
(load "~/.emacs-git.d/clojure.el")
(load "~/.emacs-git.d/markdown-mode.el")
(load "~/.emacs-git.d/org.el")
(load "~/.emacs-git.d/deft.el")
(load "~/.emacs-git.d/org-roam.el")
(load "~/.emacs-git.d/notmuch.el")
(load "~/.emacs-git.d/elfeed.el")



