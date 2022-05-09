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
   :bind (("C-c r f" . org-roam-node-find)
 	 ("C-c r r" . org-roam-node-random)
 	 ("C-c r i" . org-roam-node-insert)
 	 ("C-c o o" . org-id-get-create)
 	 ("C-c r a" . org-roam-alias-add)
 	 ("C-c r b" . org-roam-buffer-toggle)
 	 ("C-c r t" . org-roam-dailies-goto-today)
 	 ("C-c r c" . org-roam-dailies-capture-today)
 	 ("C-c r d" . org-roam-dailies-goto-date)
 	 )
   :config
   (org-roam-db-autosync-mode)
   )
