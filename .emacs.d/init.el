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

(defvar my-packages '() "Canonical list of packages.")
(setq my-packages '(
    ; Plumbing
    ivy
    counsel
    swiper
    flycheck
    flycheck-inline
    exec-path-from-shell
    company
    helpful
    imenu-list

    ; Movement
    avy
    ace-window

    ; Prettiness
    solarized-theme

    ; Document management
    deft
    org
    markdown-mode
    plantuml-mode
    graphviz-dot-mode

    ; Git
    magit
    magithub

    ;; Elixir
    alchemist

    ; Haskell
    haskell-mode
    intero

    ; Python
    company-jedi
    pipenv
    elpy

    ;; Rust
    flycheck-rust
    rust-mode
    racer
    cargo

    ;; Javascript Dev
    js2-mode
    js2-refactor
    xref-js2

    ;; Misc Languages and modes
    go-mode
    terraform-mode
    yaml-mode
    ))

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

;; Load database connections on work machine.
;; (when (string= (system-name) "mariner")
;;   (require 'databases))

(defun define-custom-global-hotkeys ()
  "Load all hotkeys related to custom modules, wher Emacs would bitch otherwise."
  (progn
    ;; Logrs
    (global-set-key (kbd "C-c l l") 'logrs-enter-log)
    (global-set-key (kbd "C-c l v") 'logrs-view-today)
    (global-set-key (kbd "C-c l y") 'logrs-view-yesterday)))

(add-hook 'after-init-hook 'load-init-settings)
(add-hook 'after-init-hook 'define-custom-global-hotkeys)

;; Replace default help with `helpful'
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;; Use IBuffer instead of Buffer-menu
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Bind IMenu-list
(setq imenu-list-focus-after-activation t)
(setq imenu-list-auto-resize t)
(global-set-key (kbd "C-c C-'") #'imenu-list-smart-toggle)

;; Movement
(global-set-key (kbd "C-:") 'avy-goto-char-2)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-x C-o") 'ace-window)
(global-set-key (kbd "C-z") 'ivy-switch-buffer)

;; Magit configuration.
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-x C-S-g") 'magithub-dashboard)

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

;; Ivy Config
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c k") 'counsel-ag)

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
(global-set-key (kbd "C-c o c") 'counsel-org-capture)

(setq org-log-done 'date)
(setq org-default-notes-file "~/Dropbox/Reference/Work/capture.org")


;; PlantUML Config
(setq plantuml-jar-path "/opt/plantuml/plantuml.jar")
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; Projectile config
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)
(setq projectile-switch-project-action #'magit-status)

;; Flycheck configuration
(exec-path-from-shell-initialize)
(global-flycheck-mode)
(with-eval-after-load 'flycheck
  (flycheck-inline-mode))

;; Company mode.
(add-hook 'after-init-hook 'global-company-mode)

;; Haskell specifics
(add-hook 'haskell-mode-hook 'intero-mode)
(setq haskell-stylish-on-save t)
(setq haskell-compile-cabal-build-command "stack build")

(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

;; Elixir specifics
(add-hook 'elixir-mode-hook 'alchemist-mode)
(setq alchemist-key-command-prefix (kbd "C-c ,"))

;; Python stuff
(elpy-enable)

;; Rust Settings
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; Javascript dev

(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
			   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(add-hook 'js2-mode-hook 'prettier-js-hook)

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
(when (string= system-type 'darwin)
  (set-face-attribute 'default nil :font "PragmataPro-15"))
(setq solarized-use-variable-pitch nil)
(load-theme 'solarized-light t)

;; Fullscreen emacs on launch on OSX.
(when (string= system-type 'darwin)
  (progn
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
  "Move point back to indentation of beginning of line.

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

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)


;; Custom compose key ;)

(global-set-key (kbd "C-c i c - >") (lambda () (interactive) (insert-char ?→))) ; For rename-commits.

;;; init.el ends here
