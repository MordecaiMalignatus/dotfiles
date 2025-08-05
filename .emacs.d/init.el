;; init.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;; Hi, I'm Az and this is my init.el.  It's a bit of a mess.  A lot of
;; things are splintered off into their own sub modules in init/, you can find a
;; list of them in `load-inits-settings'.  This is a hybrid init file that is
;; used between an OSX laptop and a Linux laptop, some things are for Linux
;; explicitly, like the SSH Agent handling, others for mac, like tool-bar-mode
;; -1.  Nearly all hotkeys are set in this file, I try not to set hotkeys in
;; modules.

;;; Code:
(require 'package)
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

(require 're-builder)
(setq reb-re-syntax 'string)

;; Disable transpose-word, the notorious troublemaker.
(global-unset-key (kbd "M-t"))

;; GPG/authinfo setup
(setq epa-pinentry-mode 'loopback)
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

(setq backup-by-copying t)              ; Don't clobber symlinks

(make-directory "~/.emacs-saves" t)
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

;; Auto-upgrade packages and delete old ones.`
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-interval 4)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-prompt-before-update t)
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
(put 'downcase-region 'disabled nil)

(use-package sqlite3
  :ensure t)

;; A better term
(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "/opt/homebrew/bin/fish"))

(use-package vterm-toggle
  :ensure t
  :config
  ;; Spawn vterm in $HOME rather than $PWD. Then we can hit C-RET to cd to file PWD.
  (setq vterm-toggle-cd-auto-create-buffer nil))

(use-package multi-vterm
  :ensure t
  :config
  (global-set-key (kbd "M-<f2>") 'multi-vterm)

  (define-key vterm-mode-map (kbd "M-N") 'vterm-toggle-forward)
  (define-key vterm-mode-map (kbd "M-P") 'vterm-toggle-backward)
  (define-key vterm-mode-map (kbd "C-<return>") #'vterm-toggle-insert-cd)

  (define-key vterm-mode-map (kbd "<f2>") 'multi-vterm-project)
  (global-set-key (kbd "<f2>") 'multi-vterm-project))

(use-package fish-mode
  :ensure t)

;; Movement
(use-package avy
  :ensure t
  :bind ("C-\\" . 'avy-goto-char-2))

(use-package ace-window
  :ensure t
  :bind (("M-o" . 'ace-window)
	 ("C-x C-o" . 'ace-window)))

(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode))

;; Ivy, auto-completion and fuzzy finder.
(use-package ivy
  :commands ivy-define-key
  :ensure t
  :bind (("C-z" . 'ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-selectable-prompt t)
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
	 ("C-x M-g" . 'magit-dispatch)))

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
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
  (exec-path-from-shell-copy-env "PATH"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infrastructure Management

(use-package puppet-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package protobuf-mode
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
  (add-hook 'org-mode-hook 'flyspell-mode)

  (setq org-log-done 'date)
  (setq org-default-notes-file "~/Sync/Reference/Work/capture.org")

  ;; Return follows a link in org, rather than line-breaking.
  (setq org-return-follows-link t)

  ;; Make org-goto less interactive, and better.
  (setq org-goto-interface 'outline-path-completionp)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)
  (setq org-reverse-note-order t) ; Refile targets top of header rather than appending.

  ;; This fixes the indent-on-tab-or-return bug that has been plaguing me.
  (setq org-src-preserve-indentation t)

  ;; Global bibliography for the stuff I read.
  (setq org-cite-global-bibliography '("~/Sync/bibliography.bib"))

  ;; enable being able to C-c C-c execute shell blocks for side effects.
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t))))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  ;; Make sure we have it available when writing blogposts.
  (define-key markdown-mode-map (kbd "C-c ]") 'az/insert-formatted-citation))

(defun az/edit-bibliography ()
  "Edit the global bibliography file."
  (interactive)
  (find-file (car org-cite-global-bibliography)))

(use-package org-ref
  :ensure t
  :config
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
  (setq bibtex-completion-bibliography '("~/Sync/bibliography.bib"))
  (setq bibtex-completion-library-path '("~/Sync/Papers/"))
  (setq bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "open" nil 0 nil fpath))))

(use-package ivy-bibtex
  :ensure t)

(defun az/insert-formatted-citation ()
  "Select a key from the bibliography, then insert citation."
  (interactive)
  (insert (bibtex-completion-apa-format-reference (org-ref-read-key))))



;; Unbind stuff from org that clogs up my free keys.
(eval-after-load "org" (progn
                         (define-key org-mode-map (kbd "C-c C-a") 'nil)))

(use-package org-ql
  :ensure t)

(defun az/setup-darwin-spellchecking ()
  "Setup hunspell and settings.

Note: the dictionary for your system's $LANG *must* be available
or Emacs' `ispell' mode does weird things. I solved this by
symlinking my desired dictionary, so that both, the default and
my desired dictionary are loaded. The notion of default here is
very weird (Albatross II got \"en_DE\"), so if it doesn't make
sense, run `(ispell-call-process ispell-program-name nil t nil
\"-D\" \"-a\" null-device)'. It will give you the error from
Emacs' perspective.

You can find the dictionaries here (yes, it's sourceforge):
https://sourceforge.net/projects/wordlist/files/speller/2020.12.07/

The full process:
1. Install hunspell
2. Grab dictionaries from Sourceforge
3. Move contained .dic and .aff file into ~/Library/Spelling
4. Rename to `en_GB.aff' and `en_GB.dic'
5. Re-run this function."
  (interactive)
  (setq ispell-program-name "hunspell")
  (setq ispell-hunspell-dict-paths-alist '(("en_GB" "~/Library/Spelling/en_GB.aff")))
  (setq hunspell-default-dict "en_GB")
  (setq ispell-local-dictionary "en_GB")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)))
  (global-set-key (kbd "M-]") 'ispell))

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
  (setq TeX-auto-save t)
  (setq bibtex-dialect 'biblatex))

(use-package reftex
  :ensure auctex
  :after latex)

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
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action #'magit-status))

;; Flycheck configuration
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  (global-set-key (kbd "C-M-n") 'flycheck-next-error)
  (global-set-key (kbd "C-M-p") 'flycheck-previous-error))

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
(defun az/go-to-language-snippets ()
  "Opens the folder in dotfiles for the snippets of `major-mode'."
  (interactive)
  (dired (concat (car yas-snippet-dirs) "/" (symbol-name major-mode))))

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
  (add-to-list 'exec-path "~/go/bin")
  (add-to-list 'exec-path "~/.asdf/shims")
  (add-to-list 'exec-path "~/.local/share/mise/shims")
  (add-to-list 'exec-path "~/.local/bin")
  (add-to-list 'exec-path "~/.rbenv/shims")
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
  (erlang-mode . lsp)
  (lsp-mode . (lambda ()
                (let ((lsp-keymap-prefix "M-l"))
                  (lsp-enable-which-key-integration))))
  :config
  (define-key lsp-mode-map (kbd "M-l") lsp-command-map)
  (define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol)
  (setq lsp-completion-provider :capf)
  (setq lsp-auto-configure t)
  (setq lsp-prefer-flymake nil)
  ;; Bash Config
  ;; Reminder: the brew formula for the bash LSP is named `bash-language-server'.
  ;; Rust Config
  (setq lsp-rust-server 'rust-analyzer)
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-clippy-preference 'on)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; Ruby Config
  ;; Server installation: gem install ruby-lsp.
  ;; Go Config
  (setq lsp-go-use-gofumpt t)
  ;; Elixir Config
  (setq lsp-elixir-ls-version "v0.23.0"))


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

(use-package erlang
  :ensure t
  :mode "\\.erl"
  :config
  (setq erlang-root-dir "/opt/local/lib/erlang")
  (add-to-list 'exec-path "/opt/local/lib/erlang/bin")
  (setq erlang-man-root-dir "/opt/local/lib/erlang/man"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python stuff
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
  :mode ("\\.rs\\'" . 'rustic-mode)
  :config
  (setq company-tooltip-align-annotations t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Golang Stuff
(require 's)

(defun az/golang-go-to-test-file ()
  "Go to the test file corresponding to file.
If none exists,create a new test file."
  (interactive)
  (let ((raw-name (s-chop-suffix ".go" (buffer-file-name))))
    (find-file (concat raw-name "_test.go"))))

(use-package go-mode
  :ensure t
  :hook (go-mode . lsp)
  :bind ("C-c f t" . 'az/golang-go-to-test-file))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

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

(use-package typescript-mode
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

(defun home-manager ()
  "Opens home.nix."
  (interactive)
  (find-file "~/dotfiles/home-manager/home.nix"))

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
    (setq mac-command-modifier 'meta)
    (az/setup-darwin-spellchecking)
    (set-face-attribute 'default nil :font "PragmataPro-13")
    (set-frame-parameter nil 'fullscreen 'fullboth)
    (global-set-key (kbd "<backtab>") #'company-complete)))

(setq-default line-spacing 0.3)
(defvar az/font-size-min 13)
(defvar az/font-size-max 17)
(defvar az/font "PragmataPro")
(defun az/toggle-font-size ()
  "Toggle font size between `az/font-size-min' and `az/font-size-max'."
  (interactive)
  (if (eq (font-get (face-attribute 'default :font) :size) az/font-size-min)
      (set-face-attribute 'default nil :font (format "%s-%d" az/font az/font-size-max))
    (set-face-attribute 'default nil :font (format "%s-%d" az/font az/font-size-min))))

;; Font rendering in X11 is fucked -- spaces in TTF files will be rendered
;; unevenly. FiraCode is an OTF font, hence the rendering is fine. It works on
;; OSX, which is why it's used there.
(when (string= system-type 'gnu/linux)
  (progn
    ;; (set-face-attribute 'default nil :font "PragmataPro-10")
    (set-face-attribute 'default nil :font "FiraCode-15")))

;; Enable/use narrow/widen.
(global-set-key (kbd "C-x w") 'widen)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-;") 'iedit-mode)

;; Bind recompile, for example for elpy's test-rerun
(global-set-key (kbd "<f12>") 'recompile)
(setq compile-command "rake")

;; Stolen from http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
;; This will turn ANSI escape codes in *compilation* to actual colour.
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)

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
(global-set-key (kbd "C-c C-q") 'bury-buffer)

(defun az/research-tasks ()
  "Open a buffer listing all research tasks that need to be done."
  (interactive)
  (switch-to-buffer "*Research TODOs*")
  (erase-buffer)
  (save-excursion (insert (shell-command-to-string "rg -A 3 -ne \"TODO(\(sar\))?\" ~/grimoire/")))
  (flush-lines "/\@archive/")
  ;; rg uses -- as separator between findings when used in non-tty mode, when I
  ;; drop the archive links the separators remain. Therefore, needing to drop
  ;; the separators.
  (flush-lines "^--$"))

(defun az/work-tasks ()
  "Open a buffer listing all `TODO(sar)' tasks in the work files."
  (interactive)
  (switch-to-buffer "*Work TODOs*")
  (erase-buffer)
  (save-excursion
    (insert (shell-command-to-string "rg -A 3 -ne \"TODO\\(sar\\):\" ~/src/"))))

;; Imenu configuration
(global-set-key (kbd "C-'") 'imenu)
(setq imenu-auto-rescan t)
(setq imenu-auto-rescan-maxout (* 1024 1024))
;; Removes the "*rescan*" option from imenu and sends it to the bottom of the list.
(setq imenu--rescan-item '("" . -99))

;; Transient!
(defun az/open-link (link)
  "Open a LINK in $BROWSER."
  (interactive)
  (shell-command (concat "open " link)))

(require 'transient)
(defun az/setup-docs-transient ()
  "Create and bind docs transient suitable for not-work."
  (transient-define-prefix docs-transient ()
    [["Local Documents"
      ("b" "Open baking.org"
       (lambda () (interactive)
         (find-file "~/Sync/Reference/Cooking/baking.org")))
      ("c" "Open cooking.org"
       (lambda () (interactive)
         (find-file "~/Sync/Reference/Cooking/cooking.org")))
      ("r" "Open reading-list.org"
       (lambda () (interactive)
         (find-file "~/Sync/Reference/Books/reading-list.org")))
      ("e" "Open events.org"
       (lambda () (interactive)
         (find-file "~/Sync/Reference/System/events.org")))
      ("a" "Open literature-archive.org"
       (lambda () (interactive)
         (find-file "~/Sync/Reference/Books/literature-archive.org")))
      ("m" "Open meal-planning.org"
       (lambda () (interactive)
         (find-file "~/Sync/Reference/Health/meal-planning.org")))
      ("f" "Open file-archive.org"
       (lambda () (interactive)
         (find-file "~/Sync/Reference/Archive/file-archive.org")))]
     ]))

(defun az/dired-open-externally ()
  "Call `open' on file under point."
  (interactive)
  (call-process "open" nil nil nil "-a VLC" (dired-get-filename nil nil)))

(define-key dired-mode-map (kbd "e") 'az/dired-open-externally)

(defun az/is-work-p ()
  "Determine if the current machine is a work machine or not."
  (file-exists-p "~/.work-machine"))

;; Work-specific config I can't check in.
;; Load it last so it overrides all that came before.
(if (az/is-work-p)
    (progn
      (load (concat user-emacs-directory "init/" "work.el")))
  (progn
    (use-package org-kasten
      :bind ("C-# C-#" . #'org-kasten-open-index)
      :defines org-kasten-home
      :config
      (setq org-kasten-home "~/Sync/Perceptron/")
      (add-hook 'org-mode-hook 'org-kasten-mode))
    (az/setup-docs-transient)
    (global-set-key (kbd "M-p") 'docs-transient)))

;; Custom compose key ;)
(global-set-key (kbd "C-c i c - >") (lambda () (interactive) (insert-char ?→))) ; For rename-commits.
(global-set-key (kbd "C-c i c < -") (lambda () (interactive) (insert-char ?←))) ; For rename-commits.

;;; init.el ends here
