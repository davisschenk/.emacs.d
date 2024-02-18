(setq package-enable-at-startup nil)
(setq inhibit-default-init nil)

;; Turn off GUI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Turn off bell
(setq ring-bell-function #'ignore
      inhibit-startup-screen t)

(provide 'early-init)
