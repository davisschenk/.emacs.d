;;; davis-commands.el --- Interactive functions for my config -*- lexical-binding: t -*-

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

(defun davis/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "New name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file." name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists." new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun davis/open-messages-buffer ()
  "Switch to Messages buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun davis/other-buffer ()
  "Switch to last buffer."
  (interactive)
  (switch-to-buffer nil))

(defun davis/zoom ()
  "Open a menu for zooming.  If LOCAL is non-nil zoom will be applied globally."
  (interactive "p")
  (unless repeat-mode (repeat-mode))
  (let ((local current-prefix-arg)
          (current-prefix-arg nil))
      (call-interactively (if local #'text-scale-adjust #'global-text-scale-adjust))))

;; Adapted from doomemacs
(defun davis/project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root
         (unless dir (bound-and-true-p projectile-project-root)))
        projectile-require-project-root)
    (projectile-project-root dir)))

;; Adapted from doomemacs
(defun davis/project-p (&optional dir)
  "Return t if DIR (defaults to `default-directory') is a valid project."
  (and (davis/project-root dir)
       t))

;; Adapted from doomemacs
(defun davis/project-find-file (dir)
  "Jump to a file in DIR (searched recursively).

If DIR is not a project, it will be indexed (but not cached)."
  (unless (file-directory-p dir)
    (error "Directory %S does not exist" dir))
  (unless (file-readable-p dir)
    (error "Directory %S isn't readable" dir))
  (let* ((default-directory (file-truename dir))
         (projectile-project-root (davis/project-root dir))
         (projectile-enable-caching projectile-enable-caching))
    (cond ((and projectile-project-root (file-equal-p projectile-project-root default-directory))
                    (unless (davis/project-p default-directory)
                      ;; Disable caching if this is not a real project; caching
                      ;; non-projects easily has the potential to inflate the projectile
                      ;; cache beyond reason.
                      (setq projectile-enable-caching nil))
                    (call-interactively #'projectile-find-file))
          ((when-let ((pr (project-current nil dir)))
             (project-find-file-in nil nil pr)))
          
          ((call-interactively #'find-file)))))

(defun davis/find-file-in-config()
  "Browse through files in the users Emacs directory."
  (interactive)
  (davis/project-find-file user-emacs-directory))

(provide 'davis-commands)
;;; davis-commands.el ends here
