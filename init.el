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

(use-package diminish
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-soft t))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-file-dispatch))
  :config
  (setq magit-define-global-key-bindings nil))

(use-package counsel
  :ensure t
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
  :ensure t)

;;;(use-package clojure-mode
;;  :ensure t
;;  :mode (("\\.clj\\'" . clojure-mode)
;;         ("\\.edn\\'" . clojure-mode)))

;;(use-package cider
;;  :ensure t)
;; Initialize and configure Org mode.

(use-package org
  :ensure t
  :init
  (setq org-log-done t)
  (setq org-skip-scheduled-if-done t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-files (list "~/org/"))
  (setq org-refile-targets '( ("~/org/projects.org" :maxlevel . 2)
			      ("~/org/inbox.org" :maxlevel . 2)
			      ("~/org/areas.org" :maxlevel . 2)
			      ("~/org/reading.org" :maxlevel . 2)
			      ("~/org/resources.org" :maxlevel . 2)
			      ("~/org/archive.org" :maxlevel . 2)))



  (defun my/get-journal-file-today (&optional visit)
    "Capture to, or optionally visit, today's journal file." 
    (interactive)
    (let* (
           (curr-date-stamp (format-time-string "%Y-%m.org"))
           (file-name (expand-file-name curr-date-stamp "~/org/")))
      (if visit
	  (switch-to-buffer (org-capture-target-buffer file-name))
      	  (set-buffer (org-capture-target-buffer file-name)))
      (goto-char (point-max))))

  (defun my/visit-journal-file-today ()
    "Visit daily journal file." 
    (interactive)
    (my/get-journal-file-today t))

  (defun my/visit-inbox ()
    (interactive)
    (find-file "~/org/inbox.org"))
  
  (setq org-tag-alist '((:startgroup . nil)
			("@work" . ?w)("@home" . ?h)
			(:endgroup . nil)
			("@note" . ?o)("@next" . ?n)("@urgent" . ?u)
			))

  
  (setq org-capture-templates
	'(("i" "Inbox TODO"
	   entry (file "~/org/inbox.org")
	   "* TODO %?  %^G\n  SCHEDULED: %t"
	   :empty-lines 1)
	  ("t" "Journal TODO"
	   entry (function my/get-journal-file-today)
	   "* TODO %?  %^g\n  SCHEDULED: %t\n  --Entered on %U\n  %i\n  %a"
	   :empty-lines 1)
	  ("j" "Daily Journal Entry"
	   entry (function my/get-journal-file-today)
	   "* %? \n  --Entered on %U\n %i\n  %a"
	   :empty-lines 1)
	  ))
  
  (defun org-dtp-open (record-location)
    "Visit the dtp message with the given Message-ID."
    (shell-command (concat "open x-devonthink-item:" record-location)))
  

  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c p j" . my/visit-journal-file-today)
	 ("C-c p i" . my/visit-inbox))
  :config
    (org-add-link-type "x-devonthink-item" 'org-dtp-open)
  )

;; (use-package org-roam
;;   :ensure t
;;   :init
;;   (setq org-roam-directory (file-truename "~/org/org-roam"))  
;;   (setq org-roam-dailies-directory "daily/")
;;   (setq org-roam-dailies-capture-templates
;; 	'(("d" "default" entry
;;            "* %?"
;;            :target (file+head "%<%Y-%m-%d>.org"
;;                               "#+title: %<%Y-%m-%d>\n"))))
;;   :bind (("C-c n f" . org-roam-node-find)
;; 	 ("C-c n r" . org-roam-node-random)
;; 	 ("C-c n i" . org-roam-node-insert)
;; 	 ("C-c n o" . org-id-get-create)
;; 	 ("C-c n a" . org-roam-alias-add)
;; 	 ("C-c n l" . org-roam-buffer-toggle)
;; 	 ("C-c t t" . org-roam-dailies-goto-today)
;; 	 ("C-c t c" . org-roam-dailies-capture-today)
;; 	 ("C-c t d" . org-roam-dailies-goto-date)
;; 	 )
;;   :config
;;   (org-roam-db-autosync-mode)
;;   )


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
