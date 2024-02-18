(setq user-full-name "Davis Schenkenberger"
      user-mail-address "davisschenk@gmail.com")

(use-package emacs
  :ensure nil
  :init
  (fset 'yes-or-no-p 'y-or-n-p)
  :custom
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (history-delete-duplicates t))

(use-package vertico
  :init 
  (vertico-mode))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :hook 
  (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize
    consult-theme :preview-key '(:debounce 0.2 any)
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-recent-file consult-xref
    consult--source-bookmark consult--source-file-register
    consult--source-recent-file consult--source-project-recent-file))
        
(use-package files
  :ensure nil
  :config
  :custom
  (require-final-newline t) ;; Make sure all files have a final newline
  (backup-by-copying t) 
  (backup-directory-alist `(("." . "~/.emacs.d/backup/")))
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 5)
  (version-control t)
  (find-file-visit-truename t))



(provide 'davis-basic)
