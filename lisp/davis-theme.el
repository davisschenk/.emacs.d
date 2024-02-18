(use-package nerd-icons)

(use-package all-the-icons)

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-treemacs-config))

  
(use-package doom-modeline
  :ensure t
  :config
  (column-number-mode 1)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-buffer-file-name-style 'file-name)
  :init (doom-modeline-mode 1))

(use-package hide-mode-line)

(provide 'davis-theme)
