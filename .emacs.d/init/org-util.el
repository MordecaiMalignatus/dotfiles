;;; package --- Summary: Az fucking with Org documents.
;;; Commentary:
;; org-util file, for fucking with my documents.
;;; Code:

(require 'org)

(defun az/org-copy-head-lines ()
  "Copy the headlines of the current buffer to the killring."
  (interactive)
  (kill-new
   (string-join (reverse
                 (org-map-entries (lambda () (org-entry-get nil "ITEM")) "LEVEL=2" 'buffer)) "\n")))

(provide 'az/org-util)
;;; org-util.el ends here
