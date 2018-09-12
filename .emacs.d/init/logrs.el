;;; logrs --- Summary
;; Logrs is a tiny tool to maintain a daily log for the things that were done,
;; or little notes you watn to review at the end of the day. These are emacs
;; bindings for that, because logrs by itself is a command-line tool, and
;; nothing more.
;;; Commentary:
;; This is a quick and dirty set of elisp functions so I don't have to keep a terminal around.

;;; Code:
(require 's)

(defun logrs-find-base-dir ()
  "Look for the file ~/.logrs and read logging filepath from it."
  (with-temp-buffer
    (insert-file-contents "~/.logrs")
    (car
     (cdr
      (split-string (buffer-string) "[\n=]" t "[[:space:]\"]*")))))

(defun logrs-enter-log (entry)
  "Log interactively entered string to logrs.
ENTRY: The string to be fed into logrs."
  (interactive "MLogrs: ")
  (shell-command (concat "logrs " (shell-quote-argument entry))))

(defun logrs-view-day (day)
  "View the log of the specified day.
DAY: The day to display, in YYYY-MM-DD format."
  (view-file (concat logrs-base-dir "/" day)))

(defun logrs-view-today ()
  "View the log for today in specific.
This is `logrs-view-today' called with the current date."
  (interactive)
  (logrs-view-day (s-trim (shell-command-to-string "date +%Y-%m-%d"))))

(defun logrs-view-yesterday ()
  "View yesterday's Logrs file."
  (interactive)
  (logrs-view-day (s-trim (shell-command-to-string "date -d 'yesterday' +%Y-%m-%d"))))

(defvar logrs-base-dir (logrs-find-base-dir)
  "Describes the folder in which logrs creates its logs, read from `~/.logrs'.")

(provide 'logrs)
;;; logrs.el ends here
