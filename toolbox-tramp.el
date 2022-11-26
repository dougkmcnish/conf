(when (file-exists-p "/usr/bin/toolbox")
  (use-package toolbox-tramp
    :straight (toolbox-tramp :type git
			     :host github
			     :repo "fejfighter/toolbox-tramp")
    :custom
    (toolbox-tramp-flatpak-wrap t)) ; Use `flatpak-spawn' when conecting
  )
