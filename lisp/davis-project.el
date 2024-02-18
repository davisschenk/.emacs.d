(use-package projectile
  :custom
  (projectile-project-search-path '("~/Developer/Work" "~/Developer/Personal" ("~/Developer/School" . 2)))
  (projectile-enable-caching t)
  :init
  (projectile-mode 1)
  :general
  (+general-global-project
    "p" 'projectile-switch-project
    "I" 'projectile-invalidate-cache
    "f" '(:ignore t :which-key "file")
    "ff" 'projectile-find-file
    "ft" 'projectile-test-files
    "C" 'projectile-compile-project
    "T" 'projectile-test-project
    "D" 'projectile-discover-projects-in-search-path
    "s" '(:ignore t :which-key "search")
    "sp" 'projectile-ripgrep))
  

(provide 'davis-project)
