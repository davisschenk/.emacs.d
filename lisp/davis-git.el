(use-package magit
  :custom
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers nil)
  (magit-revision-insert-related-refs nil)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :general
  (+general-global-git
    "g" 'magit)
  :config
  (transient-bind-q-to-quit))

(provide 'davis-git)
