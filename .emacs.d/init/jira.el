;;; jira.el --- Track the current Jira ticket  -*- lexical-binding: t; -*-

;;; Commentary:
;; A small local module that makes the "current" Jira ticket ambient state:
;;
;; * `az/jira-set-ticket' stores a ticket per git repository (persisted).
;; * The ticket is shown in the mood-line status bar.
;; * Fresh magit commit messages are prefixed with `[PROJ-123] '.
;; * New magit branches are pre-filled with `PROJ-123/'.
;;
;; When no ticket has been set explicitly for a repo, one is derived from the
;; current branch name, so a checkout of `PROJ-999/spike' needs no setup.

;;; Code:

(require 'subr-x)

;;
;; Config
;;

(defgroup az/jira nil
  "Track the current Jira ticket and weave it into magit and the mode line."
  :group 'tools
  :prefix "az/jira-")

(defcustom az/jira-ticket-regexp "[A-Z][A-Z0-9]+-[0-9]+"
  "Regexp matching a Jira ticket id such as \"PROJ-123\"."
  :group 'az/jira
  :type 'regexp)

(defcustom az/jira-commit-format "[%s] "
  "Format string applied to the ticket when inserted into a commit message."
  :group 'az/jira
  :type 'string)

(defcustom az/jira-branch-format "%s/"
  "Format string applied to the ticket when pre-filling a new branch name."
  :group 'az/jira
  :type 'string)

(defcustom az/jira-mode-line-prefix "🎫 "
  "String shown before the ticket id in the mode line.
Change this if the glyph does not render in your font."
  :group 'az/jira
  :type 'string)

(defcustom az/jira-browse-url-base "https://hometogo.atlassian.net/browse/"
  "Base URL to which a ticket id is appended to open it in a browser."
  :group 'az/jira
  :type 'string)

(defcustom az/jira-state-file (concat user-emacs-directory "jira-state.eld")
  "File in which the per-repo ticket map and history are persisted."
  :group 'az/jira
  :type 'file)

(defface az/jira-ticket-face
  '((t (:inherit mode-line-emphasis)))
  "Face for the Jira ticket indicator in the mode line."
  :group 'az/jira)

;;
;; Byte-compiler declarations
;;

(declare-function magit-get-current-branch "magit-git")
(declare-function magit-branch-and-checkout "magit-branch")
(declare-function magit-branch-create "magit-branch")
(declare-function magit-read-string-ns "magit-utils")
(declare-function transient-replace-suffix "transient")
(declare-function az/open-link "init")
(defvar git-commit-setup-hook)
(defvar magit-post-refresh-hook)

;;
;; State + resolution
;;

(defvar az/jira--ticket-alist nil
  "Alist mapping a git repository root to its explicit ticket id.")

(defvar az/jira--history nil
  "History list of Jira ticket ids entered via `az/jira-set-ticket'.")

(defun az/jira--repo-root ()
  "Return the git top-level directory for the current buffer, or nil."
  (when-let ((dir (locate-dominating-file default-directory ".git")))
    (expand-file-name dir)))

(defun az/jira--current-branch ()
  "Return the current git branch name, or nil.
Uses magit when it is loaded, otherwise shells out to git."
  (if (fboundp 'magit-get-current-branch)
      (magit-get-current-branch)
    (let ((default-directory (or (az/jira--repo-root) default-directory)))
      (with-temp-buffer
        (when (zerop (process-file "git" nil t nil
                                   "symbolic-ref" "--short" "HEAD"))
          (string-trim (buffer-string)))))))

(defun az/jira--ticket-from-branch ()
  "Extract a Jira ticket id from the current branch name, or nil."
  (when-let ((branch (az/jira--current-branch)))
    (when (string-match az/jira-ticket-regexp branch)
      (match-string 0 branch))))

(defun az/jira--extract-ticket (input)
  "Return the canonical Jira ticket id contained in INPUT, or nil.
Handles a bare id, an id embedded in surrounding text, and a Jira
URL such as \"https://host/browse/PROJ-123\" — for URLs the id in
the browse path or issue query parameter is preferred over any other
ticket-shaped token that may appear earlier in the string."
  (let ((case-fold-search t))
    (when (or (string-match
               (concat "\\(?:browse/\\|selectedIssue=\\|issueKey=\\|issues/\\)"
                       "\\(" az/jira-ticket-regexp "\\)")
               input)
              (string-match (concat "\\(" az/jira-ticket-regexp "\\)") input))
      (upcase (match-string 1 input)))))

(defun az/jira-ticket ()
  "Return the current Jira ticket id, or nil.
Prefer a ticket set explicitly for this repository, otherwise fall
back to a ticket parsed from the current branch name."
  (or (when-let ((root (az/jira--repo-root)))
        (cdr (assoc root az/jira--ticket-alist)))
      (az/jira--ticket-from-branch)))

;;
;; Persistence
;;

(defun az/jira--save-state ()
  "Persist the ticket map and history to `az/jira-state-file'."
  (ignore-errors
    (with-temp-file az/jira-state-file
      (let ((print-length nil)
            (print-level nil))
        (prin1 (list :tickets az/jira--ticket-alist
                     :history az/jira--history)
               (current-buffer))))))

(defun az/jira--load-state ()
  "Load the ticket map and history from `az/jira-state-file'."
  (when (file-readable-p az/jira-state-file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents az/jira-state-file)
        (let ((data (read (current-buffer))))
          (setq az/jira--ticket-alist (plist-get data :tickets)
                az/jira--history (plist-get data :history)))))))

;;
;; Status bar
;;

(defvar-local az/jira--modeline-cache nil
  "Cached mode-line string for the current buffer's ticket.")

(defun az/jira-refresh-mode-line (&rest _)
  "Recompute the cached Jira mode-line string for the current buffer."
  (setq az/jira--modeline-cache
        (when-let ((tid (az/jira-ticket)))
          (propertize (concat az/jira-mode-line-prefix tid)
                      'face 'az/jira-ticket-face)))
  (force-mode-line-update t))

(defun az/jira--refresh-all-buffers (&rest _)
  "Refresh the Jira mode-line cache in every live buffer."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (az/jira-refresh-mode-line))))

(defun az/jira--mode-line-string ()
  "Return the cached Jira ticket string for the mode line, or empty."
  (or az/jira--modeline-cache ""))

;;
;; Commands
;;

(defun az/jira-set-ticket (ticket)
  "Set TICKET as the current Jira ticket for this repository.
Interactively, read the ticket with completion over previously used
ids, defaulting to any ticket found in the current branch name.  The
ticket id is extracted from the input, so a bare id, a full Jira URL
\(https://host/browse/PROJ-123), or text with a leaked prompt fragment
all work."
  (interactive
   (list (completing-read "Jira ticket: " az/jira--history nil nil
                          nil 'az/jira--history (az/jira--ticket-from-branch))))
  (if-let ((found (az/jira--extract-ticket ticket)))
      (setq ticket found)
    (user-error "No Jira ticket id found in %S" ticket))
  (let ((root (or (az/jira--repo-root)
                  (user-error "Not inside a git repository"))))
    (setf (alist-get root az/jira--ticket-alist nil nil #'equal) ticket)
    (add-to-list 'az/jira--history ticket)
    (az/jira--save-state)
    (az/jira--refresh-all-buffers)
    (message "Current Jira ticket for %s: %s"
             (abbreviate-file-name root) ticket)))

(defun az/jira-clear-ticket ()
  "Remove the explicit Jira ticket set for this repository."
  (interactive)
  (if-let ((root (az/jira--repo-root)))
      (progn
        (setf (alist-get root az/jira--ticket-alist nil t #'equal) nil)
        (az/jira--save-state)
        (az/jira--refresh-all-buffers)
        (message "Cleared explicit Jira ticket for %s"
                 (abbreviate-file-name root)))
    (user-error "Not inside a git repository")))

(defun az/jira-open-ticket ()
  "Open the current Jira ticket in the browser."
  (interactive)
  (if-let ((tid (az/jira-ticket)))
      (az/open-link (concat az/jira-browse-url-base tid))
    (user-error "No current Jira ticket")))

;;
;; Commit-message embedding
;;

(defun az/jira--insert-commit-ticket ()
  "Prefix a fresh commit message with the current ticket.
Does nothing when there is no ticket, when a subject already exists
\(as with amend or reword), or when the ticket is already present."
  (when-let ((tid (az/jira-ticket)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((first-line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
        (when (and (string-empty-p (string-trim first-line))
                   (not (string-match-p (regexp-quote tid) first-line)))
          (insert (format az/jira-commit-format tid)))))))

;;
;; New-branch prefix in magit
;;

(defvar az/jira--branch-prefill nil
  "When non-nil, initial input to inject into a magit branch-name prompt.")

(defun az/jira--branch-prefill-string ()
  "Return the branch-name prefill for the current ticket, or nil."
  (when-let ((tid (az/jira-ticket)))
    (format az/jira-branch-format tid)))

(defun az/jira--read-string-ns-advice (orig prompt &optional initial-input &rest args)
  "Inject `az/jira--branch-prefill' as INITIAL-INPUT for branch-name prompts.
ORIG is the advised `magit-read-string-ns'; PROMPT and ARGS are its
remaining arguments.  Only fires while a wrapper command is active and
the prompt concerns a branch name."
  (when (and az/jira--branch-prefill
             (not initial-input)
             (string-match-p "branch" prompt))
    (setq initial-input az/jira--branch-prefill))
  (apply orig prompt initial-input args))

(defun az/jira-branch-and-checkout ()
  "Like `magit-branch-and-checkout', but pre-fill the branch name with the ticket."
  (interactive)
  (let ((az/jira--branch-prefill (az/jira--branch-prefill-string)))
    (call-interactively #'magit-branch-and-checkout)))

(defun az/jira-branch-create ()
  "Like `magit-branch-create', but pre-fill the branch name with the ticket."
  (interactive)
  (let ((az/jira--branch-prefill (az/jira--branch-prefill-string)))
    (call-interactively #'magit-branch-create)))

(defun az/jira--install-branch-suffix (loc spec)
  "Replace the `magit-branch' suffix located by LOC with SPEC.
Warn instead of erroring when LOC cannot be found, so a change in
magit's transient layout never breaks loading."
  (condition-case err
      (transient-replace-suffix 'magit-branch loc spec)
    (error (message "jira.el: could not install branch suffix %S: %s"
                    loc (error-message-string err)))))

;;
;; Setup
;;

;; Show the ticket in mood-line: it renders `mode-line-misc-info', whose
;; default value includes `global-mode-string'.
(add-to-list 'global-mode-string '(:eval (az/jira--mode-line-string)) t)

(add-hook 'find-file-hook #'az/jira-refresh-mode-line)
(add-hook 'after-save-hook #'az/jira-refresh-mode-line)

(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook #'az/jira--insert-commit-ticket))

(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook #'az/jira--refresh-all-buffers)
  (advice-add 'magit-read-string-ns :around #'az/jira--read-string-ns-advice)
  (az/jira--install-branch-suffix 'magit-branch-and-checkout
    '("c" "branch/checkout (ticket)" az/jira-branch-and-checkout))
  (az/jira--install-branch-suffix 'magit-branch-create
    '("n" "new branch (ticket)" az/jira-branch-create)))

(az/jira--load-state)
(az/jira--refresh-all-buffers)

(provide 'jira)
;;; jira.el ends here
