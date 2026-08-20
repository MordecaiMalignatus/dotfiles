;;; work --- Summary
;; Work-specific settings and files, like custom transient commands and deft prefixes.
;;; Commentary:
;; Work-specific code, this time even code I can check in!
;;; Code:
(require 'custom-deft)
(require 'jira)
(require 'work-sensitive nil 't)        ; don't error on failure-to-load

(defun az/work-deft ()
  "Override standard bindings set in custom-deft.el with work-appropriate settings."
  (setq deft-directory "~/grimoire")
  (global-set-key (kbd "C-$ C-$") (lambda () (interactive) (launch-deft-in "~/grimoire"))))

(defun az/setup-work-transient ()
  "Create and bind work transient tree."
  (transient-define-prefix work-docs-transient ()
    [[ "Local Documents"
       ("i" "open interviewing.org" (lambda () (interactive) (find-file "~/grimoire/interviewing.org")))
       ("l" "Open work-log.org" (lambda () (interactive) (find-file "~/grimoire/work-log.org")))]
     ["Links"
      ("c" "Open GCal" (lambda () (interactive) (az/open-link "https://calendar.google.com")))
      ("m" "Open GMail" (lambda () (interactive) (az/open-link "https://mail.google.com")))]
     ["Repositories"]])

  (global-set-key (kbd "M-p") 'work-docs-transient))

(defvar az/work-log-file "~/grimoire/work-log.org"
  "File the work log lives in.")

(defun az/work-log-buffer ()
  "Return the work log buffer, visiting `az/work-log-file' if needed."
  (or (get-buffer "*work-log*")
      (with-current-buffer (find-file-noselect az/work-log-file)
        (rename-buffer "*work-log*")
        (current-buffer))))

(defun az/work-log--ensure-heading (title level)
  "Find or create the heading TITLE at LEVEL in the current restriction.
Missing headings are appended at the end of the restriction.  Narrows to
the heading's subtree, so nested calls walk down the tree."
  (let ((stars (make-string level ?*)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" (regexp-quote (concat stars " " title))
                                   "[ \t]*$")
                           nil 't)
        (beginning-of-line)
      (goto-char (point-max))
      ;; Take over the trailing blank lines rather than pushing them ahead of
      ;; the new heading, so the separator below stays a single blank line.
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (insert (cond ((bobp) "")
                    ;; Sibling headings get a blank line between them, but a
                    ;; freshly created parent stays flush with its first child.
                    ((org-at-heading-p) "\n")
                    ('t "\n\n"))
              stars " " title "\n")
      (forward-line -1))
    (org-narrow-to-subtree)))

(defun az/append-to-work-log (event)
  "Append an EVENT to the work log, creating the year/month/day tree as needed."
  (interactive "sEvent to log: ")
  (with-current-buffer (az/work-log-buffer)
    (save-excursion
      (save-restriction
        (widen)
        (az/work-log--ensure-heading "Log" 1)
        (az/work-log--ensure-heading (format-time-string "%Y") 2)
        (az/work-log--ensure-heading (let ((system-time-locale "C"))
                                       (format-time-string "%B"))
                                     3)
        (az/work-log--ensure-heading (format-time-string "%F") 4)
        (goto-char (point-max))
        (skip-chars-backward " \t\n")
        (insert "\n- *" (format-time-string "%F %T") "* - " event)
        (org-fill-paragraph)
        ;; Every heading inserted above pushed the file's final newline down a
        ;; line, so collapse the end of the file back to a single one.
        (widen)
        (goto-char (point-max))
        (skip-chars-backward " \t\n")
        (delete-region (point) (point-max))
        (insert "\n")))
    (save-buffer)))

;; Setup agent shell for trial.
;; See https://github.com/xenodium/agent-shell for setup details
;; (use-package agent-shell
;;   :ensure t
;;   :config
;;   (setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config)))

(use-package acp
  :ensure t)

;; https://emacsredux.com/blog/2013/06/13/using-emacs-as-a-database-client/
(defun az/setup-sql-mode ()
  "Configure SQL modes for use."
  (setq sql-connection-alist '((pgsql-localhost (sql-product 'postgres)
                                                (sql-user "TODO: FIXME")
                                                (sql-database "cloud")
                                                (sql-server "localhost")
                                                (sql-port 9000)))))

(progn
  (az/work-deft)
  (az/setup-work-transient)
  ;; (az/setup-sql-mode)
  (global-set-key (kbd "M-'") 'az/append-to-work-log)
  (global-set-key (kbd "C-c j s") #'az/jira-set-ticket)
  (global-set-key (kbd "C-c j c") #'az/jira-clear-ticket)
  (global-set-key (kbd "C-c j o") #'az/jira-open-ticket))

(provide 'work)
;;; work.el ends here.
