;;; work --- Summary
;; Work-specific settings and files, like custom transient commands and deft prefixes.
;;; Commentary:
;; Work-specific code, this time even code I can check in!
;;; Code:
(require 'custom-deft)

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

(defun az/append-to-work-log (event)
  "Append an EVENT to the work log."
  (interactive "sEvent to log: ")
  (if (eq nil (get-buffer "*work-log*"))
      (with-current-buffer (find-file-noselect "~/grimoire/work-log.org")
        (rename-buffer "*work-log*"))
  (with-current-buffer "*work-log*"
    (goto-char (point-max))
    (if (not (s-contains? (concat "**** " (format-time-string "%F") "\n")
                          (buffer-substring-no-properties (point-min) (point-max))))
        (insert (concat "\n**** " (format-time-string "%F") "\n")))
    (insert (concat "- *" (format-time-string "%F %T") "* - " event "\n"))
    (org-fill-paragraph)
    (save-buffer))))


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
  (global-set-key (kbd "M-'") 'az/append-to-work-log))

(provide 'work)
;;; work.el ends here.
