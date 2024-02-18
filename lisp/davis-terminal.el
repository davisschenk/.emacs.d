;;; davis-terminal --- config related to terminal use
;;; Commentary:
;;; Code:

(use-package vterm
  :ensure t
  :commands (vterm vterm-other-window)
  :general
  (+general-global-open
    "t" 'vterm-other-window
    "T" 'vterm)
  :hook (vterm-mode . hide-mode-line-mode))

  
(provide 'davis-terminal)
;;; davis-terminal.el ends here
