;;; davis-org.el --- Org-mode specific configuration -*- lexical-binding: t -*-

;; Author: Davis Schenkenberger
;; Maintainer: Davis Schenkenberger


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

(use-package org)
(use-package org-mode
  :ensure nil
  :general
  (general-define-key :states '(normal) :keymaps 'org-mode-map
                      (kbd "<tab>") 'org-cycle
                      (kbd "<backtab>") 'org-shifttab)
  (general-define-key :states '(normal insert) :keymaps 'org-mode-map
                      (kbd "M-l") 'org-metaright
                      (kbd "M-h") 'org-metaleft
                      (kbd "M-k") 'org-metaup
                      (kbd "M-j") 'org-metadown
                      (kbd "M-L") 'org-shiftmetaright
                      (kbd "M-H") 'org-shiftmetaleft
                      (kbd "M-K") 'org-shiftmetaup
                      (kbd "M-J") 'org-shiftmetadown)
  (general-define-key :states  '(motion) :keymaps 'org-mode-map
                      (kbd "RET") 'org-open-at-point)
  (global-leader
    ;;for terminals
    :keymaps '(org-mode-map)
    "TAB" 'org-cycle
    "."  'org-time-stamp
    "!"  'org-time-stamp-inactive
    "<"  'org-date-from-calendar
    ">"  'org-goto-calendar

    "d"  '(:ignore t :which-key "dates")
    "dd" 'org-deadline
    "ds" 'org-schedule
    "di" 'org-time-stamp-inactive
    "dt" 'org-time-stamp

    "e"   '(:ignore t :which-key "export")
    "ee"  'org-export-dispatch

    "h"   '(:ignore t :which-key "heading")
    "hf"  'org-forward-heading-same-level
    "hb"  'org-backward-heading-same-level

    "i"  '(:ignore t :which-key "insert")
    "id" 'org-insert-drawer
    "ie" 'org-set-effort
    "if" 'org-footnote-new
    "iH" 'org-insert-heading-after-current
    "ih" 'org-insert-heading
    "ii" 'org-insert-item
    "il" 'org-insert-link
    "in" 'org-add-note
    "ip" 'org-set-property
    "is" 'org-insert-structure-template
    "it" 'org-set-tags-command

    "n"  '(:ignore t :which-key "narrow")
    "nb" 'org-narrow-to-block
    "ne" 'org-narrow-to-element
    "ns" 'org-narrow-to-subtree
    "nt" 'org-toggle-narrow-to-subtree
    "nw" 'widen

    "s"  '(:ignore t :which-key "trees/subtrees")
    "sA" 'org-archive-subtree
    "sa" 'org-toggle-archive-tag
    "sb" 'org-tree-to-indirect-buffer
    "sc" 'org-cut-subtree
    "sh" 'org-promote-subtree
    "sj" 'org-move-subtree-down
    "sk" 'org-move-subtree-up
    "sl" 'org-demote-subtree
    "sm" 'org-match-sparse-tree
    "sn" 'org-toggle-narrow-to-subtree
    "sr" 'org-refile
    "sS" 'org-sort
    "ss" '+org-sparse-tree

    "sp" '(:ignore t :which-key "priority")
    "spu" 'org-priority-up
    "spd" 'org-priority-down
    "sps" 'org-priority-show

    "t"   '(:ignore t :which-key "tables")
    "ta"  'org-table-align
    "tb"  'org-table-blank-field
    "tc"  'org-table-convert

    "td"  '(:ignore t :which-key "delete")
    "tdc" 'org-table-delete-column
    "tdr" 'org-table-kill-row
    "tE"  'org-table-export
    "te"  'org-table-eval-formula
    "tH"  'org-table-move-column-left
    "th"  'org-table-previous-field
    "tI"  'org-table-import

    "ti"  '(:ignore t :which-key "insert")
    "tic" 'org-table-insert-column
    "tih" 'org-table-insert-hline
    "tiH" 'org-table-hline-and-move
    "tir" 'org-table-insert-row
    "tJ"  'org-table-move-row-down
    "tj"  'org-table-next-row
    "tK"  'org-table-move-row-up
    "tL"  'org-table-move-column-right
    "tl"  'org-table-next-field
    "tN"  'org-table-create-with-table.el
    "tn"  'org-table-create
    "tp"  'org-plot/gnuplot
    "tr"  'org-table-recalculate
    "ts"  'org-table-sort-lines

    "tt"  '(:ignore t :which-key "toggle")
    "ttf" 'org-table-toggle-formula-debugger
    "tto" 'org-table-toggle-coordinate-overlays
    "tw"  'org-table-wrap-region

    "T"  '(:ignore t :which-key "toggle")
    "Tc"  'org-toggle-checkbox
    "Te"  'org-toggle-pretty-entities
    "TE"  '+org-toggle-hide-emphasis-markers
    "Th"  'org-toggle-heading
    "Ti"  'org-toggle-item
    "TI"  'org-toggle-inline-images
    "Tl"  'org-toggle-link-display
    "TT"  'org-todo
    "Tt"  'org-show-todo-tree
    "Tx"  'org-latex-preview
    "RET" 'org-ctrl-c-ret
    "#"   'org-update-statistics-cookies
    "'"   'org-edit-special
    "*"   'org-ctrl-c-star
    "-"   'org-ctrl-c-minus
    "A"   'org-attach))

(use-package org-indent
  :after (org)
  :ensure nil
  :hook (org-mode . org-indent-mode))

(use-package org-roam
  :ensure (org-roam :host github :repo "org-roam/org-roam")
  :after (org)
  :custom
  (org-roam-directory (file-truename "~/Developer/Notes/org-roam"))
  :general
  (+general-global-application
    "ori" 'org-roam-node-insert :which-key "Insert Node"
    "orI" 'org-id-get-create :which-key "org-id-get-create"
    "orc" 'org-roam-node-create
    "orf" 'org-roam-node-find
    "orF" 'org-roam-ref-find
    "org" 'org-roam-ui
    "orm" 'org-roam-buffer-toggle
    "orM" 'org-roam-buffer-display-dedicated
    "orr" 'org-roam-refile
    "r"  'org-roam-refile
    "R"  'org-roam-link-replace-all
    "ord" '(:ignore t :which-key "org-roam-dailies")
    "ordt" 'org-roam-dailies-goto-today :which-key "goto today"
    "ordT" 'org-roam-dailies-capture-today :which-key "capture today"
    "ordy" 'org-roam-dailies-goto-yesterday :which-key "goto yesterday"
    "ordY" 'org-roam-dailies-capture-yesterday :which-key "capture yesterday"
    "ordd" 'org-roam-dailies-goto-date :which-key "goto date"
    "ordD" 'org-roam-dailies-find-directory :which-key "find directory")

  :init
  (setq org-roam-v2-ack t)
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;; Org Exporters:
(use-package htmlize :after (org) :defer t)
(use-package ox-gfm :after (org) :defer t)
(use-package ox-twbs :after (org) :defer t)

(use-package pdf-tools
  :ensure (pdf-tools :pre-build ("./server/autobuild") :files (:defaults "server/epdfinfo"))
  :functions (pdf-isearch-batch-mode)
  :commands (pdf-tools-install pdf-view-mode)
  :config (add-hook 'pdf-view-mode-hook
                    (lambda ()
                      ;; get rid of borders on pdf's edges
                      (set (make-local-variable 'evil-normal-state-cursor) (list nil))
                      ;;for fast i-search in pdf buffers
                      (pdf-isearch-minor-mode)
                      (pdf-isearch-batch-mode)
                      (pdf-view-dark-minor-mode)
                      (pdf-view-midnight-minor-mode)))
  :mode (("\\.pdf\\'" . pdf-view-mode)))

(provide 'davis-org)

;;; davis-org.el ends here
