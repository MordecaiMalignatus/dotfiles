;;; init.el --- Summary
;;; Commentary:
;; Hi, I'm Az and this is my init.el.  It's a bit of a mess.  A lot of
;; things are splintered off into their own sub modules in init/, you can find a
;; list of them in `load-inits-settings'.  This is a hybrid init file that is
;; used between an OSX laptop and a Linux laptop, some things are for Linux
;; explicitly, like the SSH Agent handling, others for mac, like tool-bar-mode
;; -1.  Nearly all hotkeys are set in this file, I try not to set hotkeys in
;; modules.

;;; Code:
(require 'seq)

;; use-package setup
(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "use-package"))
  (require 'use-package))

(require 'package)
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(tool-bar-mode -1)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; My custom modules.
(add-to-list 'load-path (concat user-emacs-directory "init"))

(defun load-init-settings ()
  "Load custom modules concerned with things that would exceed the range of an init.el."
  (mapc 'require '(logrs
		   custom-deft)))

(add-hook 'after-init-hook 'load-init-settings)

;; Auto-upgrade packages and delete old ones.
(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

;; Replace default help functions with `helpful'
(use-package helpful
  :ensure t
  :bind (("C-h f" . #'helpful-callable)
	 ("C-h v" . #'helpful-variable)
	 ("C-h k" . #'helpful-key)))

;; Use IBuffer instead of Buffer-menu

(global-set-key (kbd "C-x C-b") #'ibuffer)

(use-package imenu-list
  :ensure t
  :bind (("C-c C-'" . #'imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t))

;; Movement
(use-package avy
  :ensure t
  :bind ("C-'" . 'avy-goto-char-2))

(use-package ace-window
  :ensure t
  :bind (("M-o" . 'ace-window)
	 ("C-x C-o" . 'ace-window)))

;; Ivy, auto-completion and fuzzy finder.
(use-package ivy
  :ensure t
  :bind (("C-z" . 'ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package swiper
  :ensure t
  :bind (("C-s" . 'swiper)))

(use-package counsel
  :ensure t
  :bind (("M-x" . 'counsel-M-x)
	 ("C-c k" . 'counsel-ag)))

;; Git, and github.
(use-package magit
  :ensure t
  :bind (("C-x g" . 'magit-status)
	 ("C-x M-g" . 'magit-dispatch-popup))
  :config
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo")))

(use-package magithub
  :ensure t
  :bind (("C-x C-M-g" . 'magithub-dashboard))
  :config
  (magithub-feature-autoinject t))

;; This package is required to copy SSH agent details from the shell, on linux.
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document management.
(use-package org
  :ensure t
  :bind (("C-c a" . 'org-agenda)
	 ("C-c o c" . 'counsel-org-capture)
	 ("C-c C-l" . 'org-store-link)
	 ("C-c l" . 'org-insert-link))
  :config
  (setq-default fill-column 80)
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'wrap-region-mode)
  (setq org-log-done 'date)
  (setq org-default-notes-file "~/Dropbox/Reference/Work/capture.org")
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate))

(use-package deft
  :ensure t)

(use-package wrap-region
  :ensure t)

;; Projectile config
(use-package projectile
  :ensure t
  :bind-keymap (("C-c p" . projectile-command-map))
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'magit-status))

;; Flycheck configuration
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package flycheck-inline
  :ensure t
  :after flycheck
  :config
  (flycheck-inline-mode))

;; Company mode.
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; Haskell specifics

(use-package haskell-mode
  :ensure t
  :bind (:map haskell-mode-map ("C-c C-c" . 'haskell-compile))
  :config
  (add-hook 'haskell-mode-hook 'intero-mode)
  (setq haskell-stylish-on-save t)
  (setq haskell-compile-cabal-build-command "stack build"))

;; Elixir specifics
(add-hook 'elixir-mode-hook 'alchemist-mode)
(setq alchemist-key-command-prefix (kbd "C-c ,"))

;; Python stuff
(use-package elpy
  :ensure t
  :config
  (elpy-enable))

;; Lispy stuff.
(use-package paredit
  :ensure t
  :bind (("C-DEL" . 'paredit-backwards-kill-word))
  :hook ((emacs-lisp-mode . paredit-mode)
	 (lisp-mode . paredit-mode)))

;; Rust Settings
(use-package rust-mode
  :ensure t
  :bind (:map rust-mode-map
	      ("TAB" . #'company-indent-or-complete-common))
  :config
  (setq company-tooltip-align-annotations t))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package racer
  :ensure t
  :hook (rust-mode)
  :config
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Javascript dev
(use-package js2-mode
  :ensure t
  :config
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (define-key js-mode-map (kbd "M-.") nil))

(use-package js2-refactor
  :after js2-mode
  :hook js2
  :ensure t
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package xref-js2
  :after js2-mode
  :ensure t)

;; Custom interactive-functions
(defun init-file ()
  "Opens init.el."
  (interactive)
  (find-file user-init-file))

(defun az/copy-filename-to-clipboard ()
  "Copies file name of current file to kill ring."
  (interactive)
  (kill-new (buffer-file-name))
  (message "Copied %s to kill ring" (buffer-file-name)))

(defun create-new-scratch-buffer ()
  "Create a new scratch buffer to work in."
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
  (switch-to-buffer (get-buffer-create bufname))
  (if (= n 1) initial-major-mode))) ; 1, because n was incremented
(global-set-key (kbd "C-x n") 'create-new-scratch-buffer)

;; Aesthetics
(menu-bar-mode 0)
(tool-bar-mode 0)

(set-face-attribute 'default nil :font "PragmataPro-12")

(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-use-variable-pitch nil)
  :config
  (load-theme 'solarized-light t))

;; Fullscreen emacs on launch on OSX.
(when (string= system-type 'darwin)
(progn
  (set-face-attribute 'default nil :font "PragmataPro-13")
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (setq ivy-use-selectable-prompt t)))

(defun az/toggle-solarized-theming ()
  "Switch between solarized-light and solarized-dark."
  (interactive)
  (cond ((custom-theme-enabled-p 'solarized-light) (progn
						     (disable-theme 'solarized-light)
						     (load-theme 'solarized-dark t)))
	((custom-theme-enabled-p 'solarized-dark) (progn
						    (disable-theme 'solarized-dark)
						    (load-theme 'solarized-light t)))))

(defun az/align-on-whitespace (start end)
  "Aligns selection on whitespace.
START: Start of region.
END: End of region."
  (interactive "r")
  (align-regexp start end "\\(\\s-*\\)\\s-" 1 0 t))

(defun az/move-beginning-of-line (arg)
  "Move point back to indentation of beginnning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.
Copied from [[https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/]]
"
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'az/move-beginning-of-line)

(defun az/search-at-point ()
  "Search for symbol at point, similar to vim's `*'."
  (interactive)
  (save-excursion
    (backward-word)
    (push-mark)
    (forward-word)
    (counsel-ag (buffer-substring (region-beginning) (region-end))
	        (file-name-directory (buffer-file-name)))
    (pop-mark)))

(global-set-key (kbd "C-c *") 'az/search-at-point)

;; Custom compose key ;)

(global-set-key (kbd "C-c i c - >") (lambda () (interactive) (insert-char ?→))) ; For rename-commits.
(global-set-key (kbd "C-c i c < -") (lambda () (interactive) (insert-char ?←))) ; For rename-commits.

;;; init.el ends here
