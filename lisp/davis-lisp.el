(use-package lispy
  :hook (emacs-lisp-mode-hook . lispy-mode)
)

(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package elisp-mode
  :ensure nil
  :general
  (global-leader
    :major-modes '(emacs-lisp-mode lisp-interaction-mode t)
    :keymaps     '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "e"  '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "ed" 'eval-defun
    "ee" 'eval-expression
    "ep" 'pp-eval-last-sexp
    "es" 'eval-last-sexp
    "i"  'elisp-index-search))

(provide 'davis-lisp)
