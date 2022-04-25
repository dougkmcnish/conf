(use-package org-roam
  :ensure t
   :init
   (setq org-roam-directory (file-truename "~/org/org-roam"))  
   (setq org-roam-dailies-directory "daily/")
   (setq org-roam-dailies-capture-templates
 	'(("d" "default" entry
            "* %?"
            :target (file+head "%<%Y-%m-%d>.org"
                               "#+title: %<%Y-%m-%d>\n"))))
   :bind (("C-c n f" . org-roam-node-find)
 	 ("C-c n r" . org-roam-node-random)
 	 ("C-c n i" . org-roam-node-insert)
 	 ("C-c n o" . org-id-get-create)
 	 ("C-c n a" . org-roam-alias-add)
 	 ("C-c n l" . org-roam-buffer-toggle)
 	 ("C-c t t" . org-roam-dailies-goto-today)
 	 ("C-c t c" . org-roam-dailies-capture-today)
 	 ("C-c t d" . org-roam-dailies-goto-date)
 	 )
   :config
   (org-roam-db-autosync-mode)
   )
