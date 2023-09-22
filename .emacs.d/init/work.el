;;; work --- Summary
;; Work-specific settings and files, like custom transient commands and deft prefixes.
;;; Commentary:
;; Work-specific code, this time even code I can check in!
;;; Code:
(require 'custom-deft)



(defun az/work-deft ()
  "Override standard bindings set in custom-deft.el with work-appropriate settings."
  (global-set-key (kbd "C-$ C-$") (lambda () (interactive) (launch-deft-in "~/grimoire"))))

(defun az/setup-work-transient ()
  "Create and bind work transient tree."
  (transient-define-prefix work-docs-transient ()
    [[ "Local Documents"
       ("i" "open interviewing.org" (lambda () (interactive) (find-file "~/grimoire/interviewing.org")))]
     ["Links"
      ("c" "Open GCal" (lambda () (interactive) (az/open-link "https://calendar.google.com")))
      ("g" "Open Github" (lambda () (interactive) (az/open-link "https://github.com/notifications")))
      ("h" "Open Honeycomb" (lambda () (interactive) (az/open-link "https://ui.honeycomb.io")))
      ("m" "Open GMail" (lambda () (interactive) (az/open-link "https://mail.google.com")))]
     ["Repositories"
      ("s" "Open tmc-stack" (lambda () (interactive) (az/open-link "https://github.com/terramate-io/tmc-stack")))
      ("g" "Open mineiros-io/iac-gcloud" (lambda () (interactive) (az/open-link "https://github.com/mineiros-io/iac-gcloud")))
      ("t" "Open terramate" (lambda () (interactive) (az/open-link "https://github.com/terramate-io/terramate")))
      ("e" "Open engineering-docs" (lambda () (interactive) (az/open-link "https://github.com/terramate-io/engineering-documentation")))
      ("l" "Open engineering-leadership-stuff" (lambda () (interactive) (az/open-link "https://github.com/mordecaimalignatus/engineering-leadership-stuff")))]])

  (global-set-key (kbd "M-p") 'work-docs-transient))

;; https://emacsredux.com/blog/2013/06/13/using-emacs-as-a-database-client/
(defun az/setup-sql-mode ()
  "Configure SQL modes for use."
  (setq sql-connection-alist '((pgsql-localhost (sql-product 'postgres)
                                                (sql-user "wi-stack-service-prd@terramate-gke-prd.iam")
                                                (sql-database "cloud")
                                                (sql-server "localhost")
                                                (sql-port 9000)))))

(progn
  (az/work-deft)
  (az/setup-work-transient)
  (az/setup-sql-mode))

(provide 'work)
;;; work.el ends here.
