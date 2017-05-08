;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'meta)
  (tool-bar-mode -1)
  (global-set-key [kp-delete] 'delete-char)) ;; sets fn-delete to be right-delete

;; package config
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Org-mode stuff.
(setq org-startup-indented 1)
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq fill-column 60)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(fill-column 80))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
