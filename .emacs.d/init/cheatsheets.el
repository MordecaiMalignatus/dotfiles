;;; cheatsheets --- Summary:
;; Inspired by a @hillelogram tweet, this is a tiny, quick and dirty minor mode
;; to enable you to use cheatsheets wherever and for whatever mode you wish.
;; The basic mode of operations is the following:
;; 1. Edit a file in `major-mode', want to look something up that you have almost certainly done in the past.
;; 2. Highlight your function/expression/thing, and press your bound hotkey.
;; 3. You are taken to `(concat user-emacs-directory "cheatsheets/" major-mode ".org")'
;; 4. If region was active, `swiper-isearch' for region content.
;;; Commentary:
;; This is extremely quick-and-dirty, but it does the job for me.  You can drop this into
;; your init folder and use it with `use-package' to load on startup:
;; (use-package cheatsheets :config (cheatsheets-mode))
;;
;; I'm bad at elisp so if you find this useful and have some improvements for it,
;; email me at mordecai at malignat.us and leave me a line or two.

;;; Code:
(defvar cheatsheets-hotkey (kbd "<f8>") "Hotkey to call cheatsheets with if active.")

(define-minor-mode cheatsheets-mode
  "A tiny minor mode to make using cheatsheets far easier."
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map cheatsheets-hotkey 'cheatsheets-init)
	    map))

(defun cheatsheets-init ()
  "Jump to cheatsheet for current `major-mode'.
If region is active, search for region in destination file."
  (interactive)
  (if (region-active-p)
      (let ((region-contents (buffer-substring (region-beginning) (region-end))))
	(cheatsheets--jump-to-file)
	(swiper-isearch region-contents))
    (cheatsheets--jump-to-file)))

(defun cheatsheets--jump-to-file ()
  "Jump to cheatsheet for current `major-mode'."
  (let ((file-path (concat user-emacs-directory "cheatsheets/" (symbol-name major-mode) ".org")))
    (find-file-other-window file-path)))

(provide 'cheatsheets)
;;; cheatsheets.el ends here
