(use-package evil
  :demand t
  :preface (setq evil-want-keybinding nil)
  :general
  (+general-global-window
    "H" 'evil-window-move-far-left
    "J" 'evil-window-move-very-bottom
    "K" 'evil-window-move-very-top
    "L" 'evil-window-move-far-right)

  (general-define-key
   :states 'visual
   "<" 'davis/evil-shift-left-visual
   ">" 'davis/evil-shift-right-visual)
   
  :custom
  (evil-symbol-word-search t "search by symbol with * and #.")
  (evil-shift-width 2 "Same behavior for vim's '<' and '>' commands")
  (evil-want-C-i-jump t)
  (evil-complete-all-buffers nil)
  (evil-want-integration t)
  (evil-want-C-i-jump t)
  (evil-search-module 'evil-search "use vim-like search instead of 'isearch")
  (evil-undo-system 'undo-redo)

  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  :config
  ;;I want Emacs regular mouse click behavior
  (define-key evil-motion-state-map [down-mouse-1] nil)
  (evil-mode 1)

  (defun davis/evil-shift-left-visual ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun davis/evil-shift-right-visual ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))
  (defun davis/update-evil-tab ()
    (setq evil-shift-width tab-width)))

  
(use-package evil-collection
  :ensure t
  :after (evil)
  :config (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-outline-bind-tab-p t))


(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package anzu)

(use-package evil-anzu
  :after (evil anzu)
  :config (global-anzu-mode 1))

(provide 'davis-evil)

