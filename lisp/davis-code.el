;;; davis-code.el --- Configuration for coding -*- lexical-binding: t -*-

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

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-seperator ?-)
  (corfu-auto t)
  :config
  (global-corfu-mode)
  (with-eval-after-load 'evil
    (setq evil-complete-next-func (lambda (_) (completion-at-point)))))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package flyspell
  :ensure nil
  :hook (prog-mode . flyspell-prog-mode)
  :hook (text-mode . flyspell-mode))

(use-package flyspell-correct
  :general 
  (general-define-key 
  	[remap ispell-word] #'flyspell-correct-at-point))

(use-package lsp-mode
  :commands lsp
  :hook (lsp-mode . flycheck-mode)
  :hook (lsp-mode . corfu-mode)
  :hook (lsp-mode . flyspell-prog-mode)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :hook (lsp-mode . lsp-diagnostics-mode)
  :hook (lsp-mode . lsp-completion-mode)

  :custom
  (lsp-diagnostics-provider :flycheck)
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :commands (lsp-ui-mode)
  :hook (lsp-mode . lsp-ui-mode))

(use-package yasnippet
  :custom (yas-snippets-dirs '(expand-file-name "snippets" user-emacs-directory))
  :init (yas-global-mode 1)
  :general
  (+general-global-insert
   "s" 'yas-insert-snippet))

(use-package yasnippet-snippets
  :after yas-snippet)


(provide 'davis-code)
;;; davis-code.el ends here
