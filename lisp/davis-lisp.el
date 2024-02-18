(use-package lispy
  :hook (emacs-lisp-mode-hook . lispy-mode)
)

(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(provide 'davis-lisp)
