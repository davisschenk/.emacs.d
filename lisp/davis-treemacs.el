;;; davis-treemacs --- config for treemacs
;;; Commentary:
;;; Code:
(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-width 25)
  :config
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  :general
  (+general-global-open
    "p" 'treemacs))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(provide 'davis-treemacs)
;;; davis-treemacs.el ends here
