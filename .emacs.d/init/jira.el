;;; jira.el --- Track the current Jira ticket  -*- lexical-binding: t; -*-

;;; Commentary:
;; A small local module that makes the "current" Jira ticket ambient state:
;;
;; * `az/jira-set-ticket' stores a ticket per git repository (persisted).
;; * The ticket is shown in the mood-line status bar.
;; * Fresh magit commit messages are prefixed with `[PROJ-123] '.
;; * New magit branches are pre-filled with `PROJ-123/', whether they come
;;   from `magit-branch-create' or from `magit-branch-spinoff'.
;;
;; When no ticket has been set explicitly for a repo, one is derived from the
;; current branch name, so a checkout of `PROJ-999/spike' needs no setup.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

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

(defcustom az/jira-branch-prefill-commands
  '(magit-branch-spinoff
    magit-branch-spinout
    magit-stash-branch
    magit-stash-branch-here)
  "Commands whose `magit-read-string-ns' read names a new branch.
These commands do not go through `magit-branch--read-name', they read
the name straight from their interactive form, so they need the prefill
hung off `magit-read-string-ns' instead.  Each of them reads exactly one
string, which is why naming the command is enough to keep the prefill
out of the other prompts these commands may raise."
  :group 'az/jira
  :type '(repeat function))

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
(declare-function magit-branch--read-name "magit-branch")
(declare-function magit-read-string-ns "magit-base")
(declare-function az/open-link "init")
(defvar git-commit-setup-hook)
(defvar magit-post-refresh-hook)

;;
;; State + resolution
;;

(defvar az/jira--ticket-alist nil
  "Alist mapping a git repository root to its explicit ticket id.")

(defvar az/jira--history nil
  "History list of Jira ticket ids entered via `az/jira-set-ticket'.
Holds canonical ids only, most recently used first; see
`az/jira--read-history' for why the raw input is kept out of it.")

(defvar az/jira--read-history nil
  "Throwaway minibuffer history for the `az/jira-set-ticket' read.
`completing-read' records what was typed verbatim in the history
variable it is handed, so handing it `az/jira--history' directly filed
pasted URLs and prompt fragments alongside the ids.  It gets this list
instead, seeded from `az/jira--history' so that M-p still walks the ids
and then thrown away; only the extracted id is recorded for real.")

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

(defvar az/jira--resolve-cache nil
  "Hash table memoising ticket resolution, or nil outside a refresh.
Bound by `az/jira--refresh-all-buffers' for the duration of one pass, so
that refreshing hundreds of buffers costs at most one git call per
repository instead of one per buffer.  Keys are repository roots, or
`default-directory' for buffers outside a repository.")

(defun az/jira--resolve-ticket ()
  "Return the Jira ticket id for the current buffer, uncached."
  (or (when-let ((root (az/jira--repo-root)))
        (cdr (assoc root az/jira--ticket-alist)))
      (az/jira--ticket-from-branch)))

(defun az/jira-ticket ()
  "Return the current Jira ticket id, or nil.
Prefer a ticket set explicitly for this repository, otherwise fall
back to a ticket parsed from the current branch name."
  (if (null az/jira--resolve-cache)
      (az/jira--resolve-ticket)
    (let* ((key (or (az/jira--repo-root) default-directory))
           (hit (gethash key az/jira--resolve-cache 'miss)))
      (if (eq hit 'miss)
          (puthash key (az/jira--resolve-ticket) az/jira--resolve-cache)
        hit))))

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

(defun az/jira--clean-history (history)
  "Return HISTORY reduced to canonical ticket ids, most recent first.
Entries are run through `az/jira--extract-ticket' and de-duplicated, so a
history persisted by an older version of this module — which could hold
pasted URLs and other raw input — collapses into the ids it meant."
  (let ((seen (make-hash-table :test #'equal))
        (clean nil))
    (dolist (entry history)
      (when-let ((tid (and (stringp entry) (az/jira--extract-ticket entry))))
        (unless (gethash tid seen)
          (puthash tid t seen)
          (push tid clean))))
    (nreverse clean)))

(defun az/jira--load-state ()
  "Load the ticket map and history from `az/jira-state-file'."
  (when (file-readable-p az/jira-state-file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents az/jira-state-file)
        (let ((data (read (current-buffer))))
          (setq az/jira--ticket-alist (plist-get data :tickets)
                az/jira--history (az/jira--clean-history
                                  (plist-get data :history))))))))

;;
;; Status bar
;;

(defvar-local az/jira--modeline-cache nil
  "Cached mode-line string for the current buffer's ticket.")

(defun az/jira--resolvable-p ()
  "Return non-nil when a ticket may safely be resolved for this buffer.
Resolution shells out to git in `default-directory', which we refuse to
do when that directory is remote (a Tramp round trip per buffer) or no
longer exists (`process-file' signals there)."
  (and (stringp default-directory)
       (not (file-remote-p default-directory))
       (file-directory-p default-directory)))

(defun az/jira--mode-line-ticket ()
  "Return the ticket to show in this buffer's mode line, or nil.
Never signals: a buffer whose ticket cannot be resolved shows nothing,
rather than aborting the refresh of every buffer that comes after it."
  (and (az/jira--resolvable-p)
       (ignore-errors (az/jira-ticket))))

(defun az/jira--update-mode-line-cache ()
  "Recompute `az/jira--modeline-cache' for the current buffer."
  (setq az/jira--modeline-cache
        (when-let ((tid (az/jira--mode-line-ticket)))
          (propertize (concat az/jira-mode-line-prefix tid)
                      'face 'az/jira-ticket-face))))

(defun az/jira-refresh-mode-line (&rest _)
  "Recompute the cached Jira mode-line string for the current buffer."
  (az/jira--update-mode-line-cache)
  (force-mode-line-update t))

(defun az/jira--refresh-all-buffers (&rest _)
  "Refresh the Jira mode-line cache in every live buffer."
  (let ((az/jira--resolve-cache (make-hash-table :test #'equal)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (az/jira--update-mode-line-cache))))
  (force-mode-line-update t))

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
   (list (let ((az/jira--read-history (copy-sequence az/jira--history)))
           (completing-read "Jira ticket: " az/jira--history nil nil
                            nil 'az/jira--read-history
                            (az/jira--ticket-from-branch)))))
  (if-let ((found (az/jira--extract-ticket ticket)))
      (setq ticket found)
    (user-error "No Jira ticket id found in %S" ticket))
  (let ((root (or (az/jira--repo-root)
                  (user-error "Not inside a git repository"))))
    (setf (alist-get root az/jira--ticket-alist nil nil #'equal) ticket)
    (setq az/jira--history (cons ticket (delete ticket az/jira--history)))
    (az/jira--save-state)
    (az/jira--refresh-all-buffers)
    (message "Current Jira ticket for %s: %s"
             (abbreviate-file-name root) ticket)))

(defun az/jira-clear-ticket ()
  "Remove the explicit Jira ticket set for this repository.
The branch-name fallback still applies afterwards, so on a branch such
as `PROJ-999/spike' the mode line keeps showing PROJ-999; the echo area
says so, to distinguish that from nothing having happened."
  (interactive)
  (if-let ((root (az/jira--repo-root)))
      (progn
        (setf (alist-get root az/jira--ticket-alist nil t #'equal) nil)
        (az/jira--save-state)
        (az/jira--refresh-all-buffers)
        (message "Cleared explicit Jira ticket for %s%s"
                 (abbreviate-file-name root)
                 (if-let ((tid (az/jira-ticket)))
                     (format " (still showing %s, from the branch name)" tid)
                   "")))
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

(defun az/jira--branch-prefill-string ()
  "Return the branch-name prefill for the current ticket, or nil."
  (when-let ((tid (az/jira-ticket)))
    (format az/jira-branch-format tid)))

(defun az/jira--branch-read-name-advice (orig prompt &rest args)
  "Prefill the current ticket into magit's new-branch NAME read.
ORIG is the advised `magit-branch--read-name'; PROMPT and ARGS are its
arguments.  The ticket is injected as initial input by wrapping
`magit-completing-read' for the duration of this one read only, so the
separate starting-point (base branch) prompt — which also uses
`magit-completing-read' — is never affected.  With no ticket, or when
magit already supplies its own initial input, behaviour is unchanged.
Covers `magit-branch-create' and `magit-branch-and-checkout' alike,
however they are invoked."
  (let ((prefill (az/jira--branch-prefill-string)))
    (if (not prefill)
        (apply orig prompt args)
      (cl-letf* ((real (symbol-function 'magit-completing-read))
                 ((symbol-function 'magit-completing-read)
                  (lambda (p coll &optional pred req init &rest more)
                    (apply real p coll pred req
                           (if (or (null init) (equal init "")) prefill init)
                           more))))
        (apply orig prompt args)))))

(defun az/jira--branch-read-string-advice (orig prompt &optional init &rest args)
  "Prefill the current ticket into new-branch reads that bypass magit's.
ORIG is the advised `magit-read-string-ns'; PROMPT, INIT and ARGS are its
arguments.  `magit-branch-spinoff' and the other commands in
`az/jira-branch-prefill-commands' read the new branch name here, from
their interactive form, so the advice on `magit-branch--read-name' never
sees them.  Gating on `this-command' — which is already the suffix
command while its interactive form runs, including when it was invoked
from a transient — keeps this out of every unrelated
`magit-read-string-ns' prompt.  An INIT supplied by the caller wins."
  (apply orig prompt
         (or (and (memq this-command az/jira-branch-prefill-commands)
                  (or (null init) (equal init ""))
                  (az/jira--branch-prefill-string))
             init)
         args))

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
  ;; Remove advice installed by earlier versions of this module, so that
  ;; reloading (rather than restarting) can never leave a stale prefill hook
  ;; attached — in particular one that leaks into the base-branch prompt.
  (advice-remove 'magit-completing-read 'az/jira--completing-read-advice)
  (advice-remove 'magit-branch-read-args 'az/jira--branch-read-args-advice)
  (advice-remove 'magit-read-string-ns 'az/jira--read-string-ns-advice)
  (if (fboundp 'magit-branch--read-name)
      (advice-add 'magit-branch--read-name :around
                  #'az/jira--branch-read-name-advice)
    (message "jira.el: `magit-branch--read-name' not found; branch prefill off"))
  (if (fboundp 'magit-read-string-ns)
      (advice-add 'magit-read-string-ns :around
                  #'az/jira--branch-read-string-advice)
    (message "jira.el: `magit-read-string-ns' not found; spin-off prefill off")))

(az/jira--load-state)
(az/jira--refresh-all-buffers)

(provide 'jira)
;;; jira.el ends here
