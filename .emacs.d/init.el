;; init.el --- Summary
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

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; My custom modules.
(add-to-list 'load-path (concat user-emacs-directory "init"))

;; Wheeeeee :D
(use-package org-kasten
  :bind ("C-# C-#" . #'org-kasten-open-index)
  :config
  (setq org-kasten-home "~/Dropbox/Perceptron/")
  (add-hook 'org-mode-hook 'org-kasten-mode))

(use-package custom-deft
  :after deft)

(use-package cheatsheets
  :config
  (cheatsheets-mode))

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
	 ("C-h k" . #'helpful-key)
	 ("C-c C-d" . #'helpful-at-point)))

;; Use IBuffer instead of Buffer-menu
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Enable disabled-by-default commands.
(put 'narrow-to-region 'disabled nil)

(use-package imenu-list
  :ensure t
  :bind (("C-c C-'" . #'imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t))

;; Movement
(use-package avy
  :ensure t
  :bind ("C-\\" . 'avy-goto-char-2))

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
  :bind (("C-s" . 'swiper-isearch)))

(use-package counsel
  :ensure t
  :bind (("M-x" . 'counsel-M-x)
	 ("C-c k" . 'counsel-rg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git, and github.
(use-package magit
  :ensure t
  :bind (("C-x g" . 'magit-status)
	 ("C-x M-g" . 'magit-dispatch))
  :config
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo")))

(use-package forge
  :after magit
  :ensure t
  :config
  (setq forge-topic-list-limit '(50 . 0)))

(use-package gist
  :ensure t)

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
	 ("C-c C-l o" . 'org-insert-link)
	 ("C-c l" . 'org-store-link)
	 ("C-c C-M-." . 'org-time-stamp-inactive)
	 ("C-x n t" . 'org-narrow-to-subtree))
  :config
  (setq-default fill-column 80)
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-log-done 'date)
  (setq org-default-notes-file "~/Dropbox/Reference/Work/capture.org")
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t))))

(use-package deft
  :ensure t)

(use-package olivetti
  :ensure t
  :config
  (global-set-key (kbd "C-c C-8 o") #'olivetti-mode))

(use-package auctex
  :defer t
  :ensure t
  :config
  (setq TeX-auto-save t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (global-flycheck-mode t))

(use-package flycheck-inline
  :ensure t
  :after flycheck
  :config
  (flycheck-inline-mode))

;; Company mode.
(use-package company
  :ensure t
  :bind (("TAB" . #'company-indent-or-complete-common))
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; YASnippets.
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
	(list (concat user-emacs-directory "snippets")))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We language servers now, awww yeh
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "M-l")
  (add-to-list 'exec-path "/Users/az/projects/elixir-ls/release/")
  :commands lsp
  :hook
  (elixir-mode . lsp)
  (rust-mode . lsp)
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-rust-server 'rust-analyzer))

(use-package lsp-ui
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

;; Add company-lsp backend for metals
(use-package company-lsp
  :ensure t)

;; Rest Client, on Taylor's recommendation
(use-package restclient
  :ensure t)

(use-package company-restclient
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nix stuff.
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elixir specifics
(use-package elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  (add-hook 'elixir-mode-hook (lambda () (define-key elixir-mode-map (kbd "<f12>") 'exunit-verify))))

(use-package exunit
  :ensure t
  :hook (elixir . exunit))

(use-package inf-elixir
  :load-path "packages/inf-elixir/"
  :bind (
         ("C-c C-l i i" . 'inf-elixir)
         ("C-c C-l i p" . 'inf-elixir-project)
         ("C-c C-l i l" . 'inf-elixir-send-line)
         ("C-c C-l i r" . 'inf-elixir-send-region)
         ("C-c C-l i b" . 'inf-elixir-send-buffer)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python stuff
(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package poetry
  :ensure t
  :config
  (poetry-tracking-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lispy stuff.

(use-package lispy
  :ensure t
  :hook ((emacs-lisp-mode . lispy-mode)
	 (lisp-mode . lispy-mode)))

;; Get you some docs
(use-package scribble-mode
  :ensure t)

(use-package racket-mode
  :ensure t
  :hook (racket . racket-xp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Golang Stuff

(use-package go-mode
  :ensure t
  :hook (go-mode . lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript Dev
(use-package js2-mode
  :ensure t
  :config
  (define-key js-mode-map (kbd "M-.") nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package js2-refactor
  :after js2-mode
  :hook js2
  :ensure t
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package xref-js2
  :after js2-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphviz and other diagram stuff.
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-auto-indent-on-braces t)
  (define-key graphviz-dot-mode-map (kbd "C-c C-p") #'graphviz-dot-preview))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(global-set-key (kbd "C-c C-a f n") 'az/copy-filename-to-clipboard)
(global-set-key (kbd "C-c C-a l n") 'global-display-line-numbers-mode)

(defun create-new-scratch-buffer ()
  "Create a new scratch buffer to work in."
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch-"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
  (switch-to-buffer (get-buffer-create bufname))
  (if (= n 1) initial-major-mode))) ; 1, because n was incremented
(global-set-key (kbd "C-x n s") 'create-new-scratch-buffer)
(setq initial-scratch-message "")

;; Aesthetics
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Force spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Coerce fullscreen on OSX
;; Enable ivy's selectable prompt on OSX because I can M-j on Linux but not OSX.
(when (string= system-type 'darwin)
  (progn
    (set-face-attribute 'default nil :font "PragmataPro-15")
    (set-frame-parameter nil 'fullscreen 'fullboth)
    (setq ivy-use-selectable-prompt t)
    (global-set-key (kbd "<backtab>") #'company-complete)))

;; Font rendering in X11 is fucked -- spaces in TTF files will be rendered
;; unevenly. FiraCode is an OTF font, hence the rendering is fine. It works on
;; OSX, which is why it's used there.
(when (string= system-type 'gnu/linux)
  (progn
     ;; (set-face-attribute 'default nil :font "PragmataPro-10")
    (set-face-attribute 'default nil :font "FiraCode-10")))

;; Enable/use narrow/widen.
(global-set-key (kbd "C-x w") 'widen)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-;") 'iedit-mode)

;; Bind recompile, for example for elpy's test-rerun
(global-set-key (kbd "<f12>") 'recompile)
(setq compile-command "rake")

(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-use-variable-pitch nil)
  :config
  (load-theme 'solarized-light t))

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
Copied from [[https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/]]"
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
    (counsel-rg (buffer-substring (region-beginning) (region-end))
	        (file-name-directory (buffer-file-name)))
    (pop-mark)))

(global-set-key (kbd "C-c *") 'az/search-at-point)

;; Custom compose key ;)

(global-set-key (kbd "C-c i c - >") (lambda () (interactive) (insert-char ?→))) ; For rename-commits.
(global-set-key (kbd "C-c i c < -") (lambda () (interactive) (insert-char ?←))) ; For rename-commits.

;;; init.el ends here
(put 'upcase-region 'disabled nil)
