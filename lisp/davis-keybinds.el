;;; davis-keybinds.el --- Configuration for keybindings and general -*- lexical-binding: t -*-

;; Author: Davis Schenkenberger
;; Maintainer: Davis Schenkenberger
;; Version: 1.0


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

(use-package general
  :demand t
  :config
  (general-override-mode)
  (general-auto-unbind-keys)
  
  (general-define-key
   :keymaps 'override
   :states '(insert normal hybrid motion visual operator emacs)
   :prefix-map '+prefix-map
   :prefix "SPC"
   :global-prefix "S-SPC")

  (general-create-definer global-definer
    :wk-full-keys nil
    :keymaps '+prefix-map)
	  
  (global-definer
    "SPC" 'find-file
    "/"   'occur
    "!"   'shell-command
    ":"   'eval-expression
    "."   'repeat
    "`"   ' davis/other-buffer
    "h"   (general-simulate-key "C-h" :which-key "help")
    "z"   'davis/zoom)
          
  (general-create-definer global-leader
    :keymaps 'override
    :states '(insert
	      normal
	      hybrid
	      motion
	      visual
	      operator)
    :prefix "SPC m"
    :non-normal-prefix "S-SPC m"
    "" '( :ignore t
          :which-key
          (lambda (arg)
            (cons (cadr (split-string (car arg)
				      " "))
                  (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))


          
  ;; https://github.com/progfolio/.emacs.d
  (defmacro +general-global-menu! (name prefix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
  Create prefix map: +general-global-NAME-map. Prefix bindings in BODY with PREFIX-KEY."
    (declare (indent 2))
    (let* ((n (concat "+general-global-" name))
           (prefix-map (intern (concat n "-map"))))
      `(progn
	 (general-create-definer ,(intern n)
           :wrapping global-definer
           :prefix-map (quote ,prefix-map)
           :prefix ,prefix-key
           :wk-full-keys nil
           "" '(:ignore t :which-key ,name))
	 (,(intern n)
	  ,@body))))
       
  
  (+general-global-menu! "buffer" "b"
    "b"   'consult-buffer
    "k"   'kill-current-buffer
    "o"   'davis/other-buffer
    "p"   'previous-buffer
    "r"   'rename-buffer
    "R"   'revert-buffer
    "M"   'davis/open-messages-buffer
    "n"   'next-buffer
    "s"   'scratch-buffer
    "TAB" 'davis/other-buffer)
          
  (+general-global-menu! "eval" "e"
    "b" 'eval-buffer
    "d" 'eval-defun
    "e" 'eval-expression
    "p" 'pp-eval-last-sexp
    "s" 'eval-last-sexp)
  
  (+general-global-menu! "file" "f"
    ;; "e" '(:ignore t :which-key "edit")
    "f" 'find-file
    "R" 'rename-file-and-buffer
    "s" 'save-buffer)
       
  (+general-global-menu! "git" "g")

  (+general-global-menu! "spelling" "s")
       
  (+general-global-menu! "narrow" "n"
    "d" 'narrow-to-defun
    "p" 'narrow-to-page
    "r" 'narrow-to-region
    "w" 'widen)
  
  (+general-global-menu! "project" "p")
  
  (+general-global-menu! "quit" "q"
    "q" 'save-buffers-kill-emacs
    "r" 'restart-emacs
    "Q" 'kill-emacs)
    
  (+general-global-menu! "open" "o"
    "P" 'davis/find-file-in-config)

  (+general-global-menu! "toggle" "t"
    "f" 'toggle-frame-fullscreen)

  (+general-global-menu! "code" "c")

  (+general-global-menu! "application" "a"
    "or" '(:ignore t :which-key "org-roam")
    "o" '(:ignore t :which-key "org-mode"))

  (+general-global-menu! "insert" "i")

  (+general-global-menu! "window" "w"
    "k" 'kill-buffer-and-window
    "d" 'delete-window
    "mm" 'delete-other-windows
    "h" 'evil-window-left
    "j" 'evil-window-down
    "k" 'evil-window-up
    "l" 'evil-window-right))

(use-package which-key
  :demand t
  :init
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33)
  (which-key-idle-delay 0.2)
  :diminish which-key-mode)
  
(elpaca-wait) ;; This is really important so other `use-package` declerations can use :general
  
(provide 'davis-keybinds)
;;; davis-keybinds.el ends here
