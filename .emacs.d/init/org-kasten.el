;;; org-kasten --- A Zettelkasten, but in plain text, using emacs as engine underneath.
;; Package-Requires: ((org-mode)(s))
;;; Commentary:
;; This is my attempt to make Zettelkaesten not suck in the digital space, so
;; I'm piggybacking on org-mode.  Org-mode is 80% there, the chiefly missing
;; thing is that it has bad navigation for this purpose.  This is mostly a bunch
;; of convenience functions atop org.
;;; Code:
(require 's)

(defvar org-kasten-home nil
  "Your home for the kasten.
If nil, org-kasten won't do anything.")

(defvar org-kasten-data-home nil
  "Directory where the card index is stored.
Defaults to user-dir.")

(defun org-kasten--file-in-kasten-p (filepath)
  "Is the file we're looking at in the kasten?
This is needed for figuring out how to deal with links.
FILEPATH: File in question."
  (s-starts-with-p (file-truename filepath)
		   (file-truename org-kasten-home)))

(defun org-kasten--parse-properties (string)
  "Get list of all regexp match in a string.
STRING: String to extract from."
  (save-match-data
    (let ((regexp "^#\\+\\(\[a-zA-Z\]+\\): \\(.*\\)")
	  (pos 0)
          matches)
      (while (string-match regexp string pos)
        (push `(,(match-string 1 string) ,(match-string 2 string)) matches)
        (setq pos (match-end 0)))
      matches)))

(defun org-kasten--read-properties ()
  (interactive)
  "Read the org-kasten relevant properties from `current-file'."
  (let ((properties (org-kasten--parse-properties (buffer-string)))
	(id (make-local-variable 'org-kasten-id))
	(links (make-local-variable 'org-kasten-links)))
    (setq id  (alist-get "ID" properties))
    (setq links (split-string (alist-get "LINKS" properties)))))

;; TODO This isn't creating a proper alist. Make sure it does so we can retrieve the properties.
(alist-get "LINKS") (org-kasten--extract-properties "#+ID: 1
#+LINKS: 2 1 5

This is a simple card that I use for checking purposes.
")

(provide 'org-kasten)
;;; org-kasten.el ends here
