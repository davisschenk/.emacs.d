;;; davis-treemacs --- config for treemacs
;;; Commentary:
;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-width 25)
  (treemacs-follow-after-init t)
  (treemacs-is-never-other-window t)

  :config
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-follow-mode -1)

  ;; Adapted from doomemacs
  (defun +treemacs/toggle ()
    "Initialize or toggle treemacs.

  Ensures that only the current project is present and all other projects have
  been removed.

  Use `treemacs' command for old functionality."
    (interactive)
    (require 'treemacs)
    (pcase (treemacs-current-visibility)
      (`visible (delete-window (treemacs-get-local-window)))
      (_ (if (davis/project-p)
            (treemacs-add-and-display-current-project)
          (treemacs)))))

  :general
  (+general-global-open
    "p" '+treemacs/toggle))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-nerd-icons
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))


(provide 'davis-treemacs)
;;; davis-treemacs.el ends here
