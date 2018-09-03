(require 'package)
(add-to-list 'package-archives '("org"          . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(tool-bar-mode -1)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(add-to-list 'load-path (concat user-emacs-directory "init-modules"))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Magit configuration.
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-z") 'list-bookmarks)

;; Handle SSH-agent for magit
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

;; Wrap-region mode.
(wrap-region-add-wrappers
 '(("$" "$" nil '(org-mode markdown-mode))
   ("`" "`" nil '(markdown-mode))))

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

;; Deft configuration
(defun launch-deft-in (dir)
  "Launch Deft in specified directory.
DIR: The directory that deft should treat as `deft-directory`"
  (let (old-dir deft-directory)
    (setq deft-directory dir)
    (relaunch-deft)
    (setq deft-directory old-dir)))

(defun relaunch-deft ()
  "Relaunch deft instead of just switching back to it."
  (interactive)
  (when (get-buffer "*Deft*")
    (kill-buffer "*Deft*"))
  (deft))

(global-set-key (kbd "C-$ C-$") '(lambda () (interactive)(launch-deft-in "~/Dropbox/Reference")))
(global-set-key (kbd "C-$ p")   '(lambda () (interactive)(launch-deft-in "~/Dropbox/Perceptron")))
(global-set-key (kbd "C-$ w")   '(lambda () (interactive)(launch-deft-in "~/Dropbox/Reference/Work")))

(setq deft-directory "~/Dropbox/Reference")
(setq deft-use-filename-as-title t)
(setq deft-recursive t)
