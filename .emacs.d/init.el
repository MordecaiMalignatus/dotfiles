;;; init.el --- Summary
;;; Commentary:
;; Hi, I'm Mordecai and this is my init.el.  It's a bit of a mess.  A lot of
;; things are splintered off into their own sub modules in init/, you can find a
;; list of them in `load-inits-settings'.  This is a hybrid init file that is
;; used between an OSX laptop and a Linux laptop, some things are for Linux
;; explicitly, like the SSH Agent handling, others for mac, like tool-bar-mode
;; -1.  Nearly all hotkeys are set in this file, I try not to set hotkeys in
;; modules.

;;; Code:
(require 'package)
(require 'exec-path-from-shell)


;; Packages.
(add-to-list 'package-archives '("org"  . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa". "https://melpa.org/packages/") t)
(package-initialize)

(tool-bar-mode -1)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; My custom modules.
(add-to-list 'load-path (concat user-emacs-directory "init"))

(defun load-init-settings ()
  "Load custom modules concerned with things that would exceed the range of an init.el."
  (mapc 'require '(logrs
		   custom-deft)))

(add-hook 'after-init-hook 'load-init-settings)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Magit configuration.
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-z") 'list-bookmarks)

;; Logrs
(global-set-key (kbd "C-c l l") 'logrs-enter-log)
(global-set-key (kbd "C-c l v") 'logrs-view-today)
(global-set-key (kbd "C-c l y") 'logrs-view-yesterday)

;; Handle SSH-agent for magit

(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

;; Org-Mode Config.
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'wrap-region-mode)

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done 'date)

;; Flycheck configuration
(exec-path-from-shell-initialize)
(global-flycheck-mode)

;; Company mode.
(add-hook 'after-init-hook 'global-company-mode)

;; Haskell specifics
(add-hook 'haskell-mode-hook 'intero-mode)
(setq haskell-stylish-on-save t)
(setq haskell-compile-cabal-build-command "stack build")

(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

;; Custom interactive-functions
(defun init-file ()
  "Opens init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Aesthetics
(set-face-attribute 'default nil :font "PragmataPro-13")
(setq solarized-use-variable-pitch nil)
(load-theme 'solarized-light t)

(defun pocket-pop-article ()
  "Pops a single article off of my pocket queue and opens it in the browser."
  (interactive)
  (shell-command "pockyt get -n 1 -r oldest -s unread -o browser | pockyt mod -a 1 -i redirect"))

(global-set-key (kbd "C-x p p") 'pocket-pop-article)

;;; init.el ends here
