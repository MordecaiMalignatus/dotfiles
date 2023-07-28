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
      ("s" "open slack-integration.org" (lambda () (interactive) (find-file "~/grimoire/Projects/slack-integration.org")))
      ("i" "open interviewing.org"      (lambda () (interactive) (find-file "~/grimoire/interviewing.org")))]
     ["Links"
      ("c" "Open GCal" (lambda () (interactive) (az/open-link "https://calendar.google.com")))
      ("g" "Open Github" (lambda () (interactive) (az/open-link "https://github.com/notifications")))
      ("h" "Open Honeycomb" (lambda () (interactive) (az/open-link "https://ui.honeycomb.io")))
      ("m" "Open GMail" (lambda () (interactive) (az/open-link "https://mail.google.com")))]])

  (global-set-key (kbd "M-p") 'work-docs-transient))

(progn
  (az/work-deft)
  (az/setup-work-transient))

(provide 'work)
;;; work.el ends here.
