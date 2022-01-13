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
;; Quelpa Setup and use-package bootstrapping.
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Infrastructure
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)
(setq sentence-end-double-space nil)

(setq backup-by-copying t)              ; Don't clobber symlinks

(make-directory "~/.emacs-saves")
;;  no more littered *~ files everywhere.
(setq backup-directory-alist '(("." . "~/.emacs-saves")))

;; My custom modules.
(add-to-list 'load-path (concat user-emacs-directory "init"))

(use-package custom-deft
  :after deft)

(use-package cheatsheets
  :defines cheatsheets-directory
  :functions cheatsheets-mode
  :config
  (setq cheatsheets-directory "~/grimoire/cheatsheets/")
  (cheatsheets-mode))

;; Auto-upgrade packages and delete old ones.
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-interval 4)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(setq quelpa-upgrade-interval 7)
(add-hook #'after-init-hook #'quelpa-upgrade-all-maybe)

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
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; A better term
(use-package vterm
  :ensure t)

(use-package vterm-toggle
  :ensure t
  :config
  ;; Spawn vterm in $HOME rather than $PWD. Then we can hit C-RET to cd to file PWD.
  (setq vterm-toggle-cd-auto-create-buffer nil)
  (define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)

  (global-set-key (kbd "<f2>") 'vterm-toggle)
  (define-key vterm-mode-map (kbd "<f2>") 'vterm-toggle)
  (define-key vterm-mode-map (kbd "M-N") 'vterm-toggle-forward)
  (define-key vterm-mode-map (kbd "M-P") 'vterm-toggle-backward))

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
;; Infrastructure Management

(use-package puppet-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document management.
(use-package org
  :ensure t
  :bind (("C-c a" . 'org-agenda)
	 ("C-c o c" . 'counsel-org-capture)
	 ("C-c C-l o" . 'org-insert-link)
	 ("C-c l" . 'org-store-link)
         ("C-c C-'" . 'org-cycle-agenda-files)
         ("C-'" . 'imenu)
	 ("C-c C-M-." . 'org-time-stamp-inactive)
	 ("C-x n t" . 'org-narrow-to-subtree))
  :defines (org-goto-interface org-outline-path-complete-in-steps)
  :functions org-insert-link
  :config
  (setq-default fill-column 80)

  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'org-mode-hook 'org-indent-mode)

  (setq org-log-done 'date)
  (setq org-default-notes-file "~/Sync/Reference/Work/capture.org")

  ;; Return follows a link in org, rather than line-breaking.
  (setq org-return-follows-link t)

  ;; Make org-goto less interactive, and better.
  (setq org-goto-interface 'outline-path-completionp)
  (setq org-outline-path-complete-in-steps nil)

  ;; enable being able to C-c C-c execute shell blocks for side effects.
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t))))

;; Unbind stuff from org that clogs up my free keys.
(eval-after-load "org" (progn
                        (define-key org-mode-map (kbd "C-c c-a") 'nil)))


(use-package deft
  :ensure t)

(use-package olivetti
  :ensure t
  :config
  (global-set-key (kbd "C-c C-8") #'olivetti-mode))

(use-package auctex
  :defer t
  :ensure t
  :defines TeX-auto-save
  :config
  (setq TeX-auto-save t))

(use-package yaml-mode
  :ensure t)

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

(use-package restclient
  :ensure t )

(use-package company-restclient
  :ensure t )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We language servers now, awww yeh
(use-package lsp-mode
  :ensure t
  :init
  (add-to-list 'exec-path "~/.local/elixir-ls/")
  (add-to-list 'exec-path "~/go/bin")
  (add-to-list 'exec-path "~/.asdf/shims")
  (add-to-list 'exec-path "~/.local/bin")
  :commands lsp
  :defines (lsp-completion-provider
            lsp-prefer-flymake
            lsp-rust-server
            lsp-rust-clippy-preference
            lsp-rust-analyzer-cargo-watch-command)
  :bind (("C-c C-d" . lsp-describe-thing-at-point))
  :bind-keymap ("M-l" . lsp-command-map)
  :hook
  (elixir-mode . lsp)
  (rustic-mode . lsp)
  (ruby-mode . lsp)
  (go-mode . lsp)
  (sh-mode . lsp)
  (lsp-mode . (lambda ()
                (let ((lsp-keymap-prefix "M-l"))
                  (lsp-enable-which-key-integration))))
  :config
  (define-key lsp-mode-map (kbd "M-l") lsp-command-map)
  (setq lsp-completion-provider :capf)
  (setq lsp-auto-configure t)
  (setq lsp-prefer-flymake nil)
  ;; Rust Config
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-rust-clippy-preference 'on)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"))


(use-package lsp-ui
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

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
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package exunit
  :ensure t
  :hook (elixir . exunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python stuff
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))
;; Typechecking
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package poetry
  :ensure t
  :hook
  (setq poetry-tracking-strategy 'projectile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lispy stuff.

(use-package lispy
  :ensure t
  :hook ((emacs-lisp-mode . lispy-mode)
	 (lisp-mode . lispy-mode)))

(defvar show-paren-delay)
(setq show-paren-delay 0)
(show-paren-mode)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (require 'rainbow-delimiters)
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66") ; red
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6") ; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f") ; blue
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6") ; yellow
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc") ; cyan
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c") ; magenta
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc") ; light gray
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999") ; medium gray
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666")) ; dark gray

;; Get you some docs
(use-package scribble-mode
  :ensure t)

(use-package racket-mode
  :ensure t
  :hook (racket . racket-xp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust Settings
(use-package rustic
  :ensure t
  :config
  (setq company-tooltip-align-annotations t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Golang Stuff

(use-package go-mode
  :ensure t
  :hook (go-mode . lsp))

(use-package flycheck-golangci-lint
  :ensure t)

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
;; Lua Stuff

(use-package lua-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  (setq lua-indent-level 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphviz and other diagram stuff.
(use-package graphviz-dot-mode
  :ensure t
  :defines graphviz-dot-auto-indent-on-braces
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

(defun az/create-new-scratch-buffer ()
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
(global-set-key (kbd "C-x n s") 'az/create-new-scratch-buffer)
(setq initial-scratch-message "")

;; Aesthetics
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(use-package mood-line
  :ensure nil
  :config
  (mood-line-mode))

;; Force spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Disable .#FILE# files.
(setq create-lockfiles nil)

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
  (setq solarized-scale-markdown-headlines t)
  (setq solarized-use-more-italic t)
  :config
  (load-theme 'solarized-selenized-dark t))

(defun az/toggle-solarized-theming ()
  "Switch between solarized-light and solarized-dark."
  (interactive)
  (cond ((custom-theme-enabled-p 'solarized-selenized-light) (progn
						     (disable-theme 'solarized-selenized-light)
						     (load-theme 'solarized-selenized-dark t)))
	((custom-theme-enabled-p 'solarized-selenized-dark) (progn
						    (disable-theme 'solarized-selenized-dark)
						    (load-theme 'solarized-selenized-light t)))))

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
(global-set-key (kbd "C-c C-b") 'bury-buffer)

;; Imenu configuration
(global-set-key (kbd "C-'") 'imenu)
(setq imenu-auto-rescan t)
(setq imenu-auto-rescan-maxout (* 1024 1024))
;; Removes the "*rescan*" option from imenu and sends it to the bottom of the list.
(setq imenu--rescan-item '("" . -99))

;; Work-specific config I can't check in.
;; Load it last so it overrides all that came before.
(if (string= (system-name) "ALT00622")
    (progn
      (load (concat user-emacs-directory "init/" "work.el")))
  (progn
    (use-package org-kasten
      :bind ("C-# C-#" . #'org-kasten-open-index)
      :defines org-kasten-home
      :config
      (setq org-kasten-home "~/Sync/Perceptron/")
      (add-hook 'org-mode-hook 'org-kasten-mode))))


;; Custom compose key ;)

(global-set-key (kbd "C-c i c - >") (lambda () (interactive) (insert-char ?→))) ; For rename-commits.
(global-set-key (kbd "C-c i c < -") (lambda () (interactive) (insert-char ?←))) ; For rename-commits.

;;; init.el ends here
