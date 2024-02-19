;;; davis-terminal --- config related to terminal use
;;; Commentary:
;;; Code:

(use-package vterm
  :ensure t
  :commands (vterm vterm-other-window)
  :hook (vterm-mode . hide-mode-line-mode))

(use-package vterm-toggle
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-project-root t)
  (vterm-toggle-cd-auto-create-buffer t)

  :general
  (+general-global-open
    "t" 'vterm-toggle-cd :which-key "Toggle terminal"))

  
(provide 'davis-terminal)
;;; davis-terminal.el ends here
