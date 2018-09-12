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
(require 'seq)

;; Packages.
(require 'package)

(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(tool-bar-mode -1)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Making sure everything is installed.

(defvar my-packages
  '(
    ; Plumbing
    ivy
    counsel
    swiper
    flycheck
    exec-path-from-shell
    company

    ; Movement
    avy
    ace-window

    ; Prettiness
    gruvbox-theme
    solarized-theme

    ; Document management
    deft
    org

    ; Git
    magit
    magithub

    ; Haskell
    haskell-mode
    intero

    ; Python
    company-jedi
    pipenv
    )
  "Canonical list of packages.")

(defun my-packages-in-sync-p ()
  "Decide if all packages in `my-packages' are installed."
  (seq-every-p 'package-installed-p my-packages))

(defun install-my-packages ()
  "Make sure every package in `my-packages' is installed from remotes."
  (interactive)
  (unless (my-packages-in-sync-p)
    (message "%s" "Packages out of sync, refreshing contents...")
    (package-refresh-contents)
    (message "%s" "Refresh done. Updating missing packages...")
    (mapc #'(lambda (package-name)
	      (unless (package-installed-p package-name)
		(package-install package-name)))
	  my-packages)))

(add-hook 'after-init-hook 'install-my-packages)

;; My custom modules.
(add-to-list 'load-path (concat user-emacs-directory "init"))

(defun load-init-settings ()
  "Load custom modules concerned with things that would exceed the range of an init.el."
  (mapc 'require '(logrs
		   custom-deft)))

(defun define-custom-global-hotkeys ()
  "Load all hotkeys related to custom modules, wher Emacs would bitch otherwise."
  (progn
    ;; Logrs
    (global-set-key (kbd "C-c l l") 'logrs-enter-log)
    (global-set-key (kbd "C-c l v") 'logrs-view-today)
    (global-set-key (kbd "C-c l y") 'logrs-view-yesterday)))

(add-hook 'after-init-hook 'load-init-settings)
(add-hook 'after-init-hook 'define-custom-global-hotkeys)

;; Movement
(global-set-key (kbd "C-:") 'avy-goto-char-2)
(global-set-key (kbd "M-o") 'ace-window)

;; Magit configuration.
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-z") 'list-bookmarks)

(setq auth-sources '("~/.authinfo.gpg"))

;; Ivy Config
;; This is experimental and I'm not sure it's going to stay.
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)

;; Handle SSH-agent for magit
(require 'exec-path-from-shell)
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

;; Projectile config
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)

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
