;;; org-kasten --- A Zettelkasten, but in plain text, using emacs as engine underneath.
;; Package-Requires: ((org-mode)(s)(dash))
;;; Commentary:
;; This is my attempt to make Zettelkaesten not suck in the digital space, so
;; I'm piggybacking on org-mode.  Org-mode is 80% there, the chiefly missing
;; thing is that it has bad navigation for this purpose.  This is mostly a bunch
;; of convenience functions atop org.
;;; Code:
(require 's)
(require 'dash)

(defvar org-kasten-home "~/.emacs.d/init/example-kasten/"
  "Your home for the kasten.
If nil, org-kasten won't do anything.")

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
        (push `(,(match-string 1 string) . ,(match-string 2 string)) matches)
        (setq pos (match-end 0)))
      matches)))


(defun org-kasten--read-properties ()
  "Read the org-kasten relevant properties from `current-file'."
  (interactive)
  (let* ((buffer-text (buffer-substring-no-properties (point-min) (point-max)))
         (properties  (org-kasten--parse-properties buffer-text)))
    (setq-local org-kasten-id    (cdr (assoc "ID" properties)))
    (setq-local org-kasten-links (split-string (cdr (assoc "LINKS" properties))))))


(defun org-kasten--find-file-for-index (index)
  "Convert a link INDEX as number or string to a full filepath."
  (if (not (string= nil org-kasten-home))
      (let* ((files-in-kasten           (-drop 2 (directory-files org-kasten-home)))
	     (string-index              (if (numberp index)
				            (number-to-string index)
				            index))
	     (files-starting-with-index (-filter (lambda (file) (s-starts-with-p string-index file))
						 files-in-kasten)))
	;; TODO: This needs to error if there is no file, the kasten is inconsistent, then.
	(if (> (length files-starting-with-index) 1)
	    (error (concat "Org-Kasten inconsistent, multiple files with index " string-index))
	  (car files-starting-with-index)))))

(defun org-kasten-navigate-links ()
  "Navigate to one of the links from the current card."
  (interactive)
  (org-kasten--read-properties)
  ;; TODO: This is hilariously inefficient, find a better way.
  (let* ((files (mapcar 'org-kasten--find-file-for-index org-kasten-links)))
    (find-file (completing-read "Links:" files))))

(defun org-kasten-new-note ()
  "Create a new, enumerated note in the Kasten."
  (interactive))

(defun org-kasten-open-index ()
  "Open your index and link file."
  (interactive)
  (find-file (concat org-kasten-home "/0-index.org")))

(defun org-kasten-create-child ()
  "Create a new card that is linked to this one."
  (interactive))

(defun org-kasten-add-link ()
  "Link this card with another one."
  (interactive))

(defun org-kasten-remove-link ()
  "Remove an existing link between this card and another."
  (interactive))

(defun org-kasten-delete-card ()
  "Delete a card and all of its links.
Can be useful, if it's useful too often you might need to reconsider."
  (interactive))

(provide 'org-kasten)
;;; org-kasten.el ends here
