;;; puppet-setup.el --- Puppet language server and formatting  -*- lexical-binding: t; -*-

;;; Commentary:
;; Two separate concerns, because puppet-editor-services -- the only real
;; Puppet language server -- does not implement textDocument/formatting:
;;
;; * LSP: puppetlabs/puppet-editor-services, registered by hand because
;;   `lsp-mode' ships no Puppet client.  Provides diagnostics (puppet parser
;;   plus puppet-lint), hover, completion, jump-to-definition, document and
;;   workspace symbols, folding, and arrow alignment as you type `>'.
;;
;;   The server is looked for on `exec-path' first, so `gem install
;;   puppet-editor-services' transparently takes over from the checkout in
;;   `az/puppet-editor-services-directory' once it is installed.  Either way
;;   it needs Ruby 3.1 or newer, which is why the checkout is started as an
;;   argument to an explicitly chosen interpreter -- see
;;   `az/puppet-languageserver-command'.
;;
;; * Formatting: `puppet-lint --fix' -- which sorts out arrow alignment, quote
;;   style and ensure-first ordering, but not indentation -- followed by a
;;   reindent that steps around heredocs.

;;; Code:
(require 'cl-lib)
(require 'lsp-mode)
(require 'projectile)
(require 'puppet-mode)
(require 'reformatter)

(defgroup az/puppet nil
  "Puppet language server and formatting."
  :group 'puppet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language server

(defcustom az/puppet-editor-services-directory
  (expand-file-name "~/github_clones/puppet-editor-services")
  "Checkout of puppetlabs/puppet-editor-services.
Only consulted when `puppet-languageserver' is not already on `exec-path'."
  :group 'az/puppet
  :type 'directory)

(defcustom az/puppet-ruby-executable nil
  "Ruby interpreter that runs the language server checkout.
nil means the first `ruby' on `exec-path', which -- with the mise and rbenv
shims sitting at the front of it -- is the version-managed one.  It has to be
3.1 or newer: puppet-editor-services declares that as its
`required_ruby_version' and uses the shorthand hash syntax that older parsers
reject outright."
  :group 'az/puppet
  :type '(choice (const :tag "First ruby on `exec-path'" nil)
                 (file :must-match t)))

(defun az/puppet-ruby ()
  "Return the Ruby interpreter to run the language server checkout with."
  (or az/puppet-ruby-executable (executable-find "ruby") "ruby"))

(defun az/puppet-languageserver-executable ()
  "Return the path to `puppet-languageserver', or nil when it is not installed."
  (or (executable-find "puppet-languageserver")
      (let ((script (expand-file-name "puppet-languageserver"
                                      az/puppet-editor-services-directory)))
        (and (file-readable-p script) script))))

(defun az/puppet-languageserver-checkout-p (executable)
  "Say whether EXECUTABLE lives in `az/puppet-editor-services-directory'.
The checkout tends to be on PATH too, so finding the server on `exec-path'
is no proof that we found a gem-installed one."
  (file-in-directory-p executable az/puppet-editor-services-directory))

(defun az/puppet-languageserver-command ()
  "Return the command line that starts the Puppet language server.

The checkout is passed to `az/puppet-ruby' rather than executed directly:
its shebang says /usr/bin/env ruby, and the PATH Emacs hands to subprocesses
is not the one an interactive shell has -- it finds the macOS system Ruby 2.6
first, which cannot even parse the server sources.  A gem-installed
`puppet-languageserver' already carries a shebang for the Ruby it was
installed under, so that one is run as it is.

--timeout=0 keeps the server from giving up on us: it loads all of Puppet's
types and functions in the background, which takes some twenty seconds on a
cold start."
  (let ((server (az/puppet-languageserver-executable))
        (args (list "--stdio" "--timeout=0")))
    (cond ((null server) (cons "puppet-languageserver" args))
          ((az/puppet-languageserver-checkout-p server)
           (append (list (az/puppet-ruby) server) args))
          (t (cons server args)))))

(add-to-list 'lsp-language-id-configuration '(puppet-mode . "puppet"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   #'az/puppet-languageserver-command
                   (lambda () (and (az/puppet-languageserver-executable) t)))
  :activation-fn (lsp-activate-on "puppet")
  :server-id 'puppet-editor-services
  :priority -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting

(defun az/puppet-project-root ()
  "Return the root of the control repo or module around the current buffer."
  (or (projectile-project-root) default-directory))

;; puppet-lint reads .puppet-lint.rc from its working directory, so run it from
;; the project root rather than from the manifest's own directory.  It writes
;; its fixes back into the file it is handed, hence :stdin and :stdout nil.
(reformatter-define az/puppet-lint-fix
  :program "puppet-lint"
  :args (list "--fix" input-file)
  :stdin nil
  :stdout nil
  :mode nil
  :input-file (reformatter-temp-file "pp")
  :working-directory (az/puppet-project-root)
  ;; puppet-lint exits non-zero whenever it leaves anything unfixed, which is
  ;; the normal case -- undocumented classes, autoloader layout and so on are
  ;; reported but not fixable.  When it cannot parse the manifest at all it
  ;; leaves the file untouched, so accepting every exit code costs us nothing.
  :exit-code-success-p (lambda (_) t))

(defun az/puppet-heredoc-body-lines ()
  "Return a hash table of the line numbers that fall inside a heredoc.
`puppet-mode' has no idea heredocs exist and happily reindents their
contents, which silently rewrites the string.  The terminator line is
included because its indentation is the `|' strip margin."
  (let ((lines (make-hash-table :test #'eql)))
    (save-excursion
      (goto-char (point-min))
      ;; Openers look like @(EOT), @("EOT"), @("EOT":json/nrt) and so on.
      (while (re-search-forward "@(\\s-*\"?\\([A-Za-z0-9_.-]+\\)\"?[^)\n]*)" nil t)
        (let ((tag (match-string 1)))
          (forward-line 1)
          (let ((from (line-number-at-pos)))
            (if (re-search-forward (concat "^[ \t]*|?[ \t]*-?[ \t]*"
                                           (regexp-quote tag) "[ \t]*$")
                                   nil t)
                (cl-loop for line from from to (line-number-at-pos)
                         do (puthash line t lines))
              ;; Unterminated heredoc: treat the rest of the buffer as string.
              (goto-char (point-max)))))))
    lines))

(defun az/puppet-indent-buffer ()
  "Reindent the whole buffer, leaving heredoc bodies alone."
  (interactive)
  (let ((skip (az/puppet-heredoc-body-lines))
        (line 1))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (unless (or (gethash line skip)
                    ;; Indenting a blank line only buys trailing whitespace.
                    (looking-at-p "[ \t]*$"))
          (indent-according-to-mode))
        (setq line (1+ line))
        (forward-line 1)))))

(defun az/puppet-format-buffer ()
  "Format the current manifest with `puppet-lint --fix', then reindent it."
  (interactive)
  (if (executable-find "puppet-lint")
      (az/puppet-lint-fix-buffer)
    (message "puppet-lint is not on exec-path, only reindenting"))
  (az/puppet-indent-buffer))

(defun az/puppet-format-on-save ()
  "Format the buffer before saving.  Meant for `puppet-mode-hook'."
  (add-hook 'before-save-hook #'az/puppet-format-buffer nil t))

(define-key puppet-mode-map (kbd "C-c C-f") #'az/puppet-format-buffer)

(add-hook 'puppet-mode-hook #'lsp)
(add-hook 'puppet-mode-hook #'az/puppet-format-on-save)

(provide 'puppet-setup)
;;; puppet-setup.el ends here
