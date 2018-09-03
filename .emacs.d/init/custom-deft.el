;;; custom-deft --- Summary
;; A bunch of functions to make Deft work nicely with more than just the static
;; environment it envisions.
;;;;;;;;;;;;;
;;; Commentary:
;; For questions or comments email mordecai at malignat dot us

;;; Code:
(defun launch-deft-in (dir)
  "Launch Deft in specified directory.
DIR: The directory that deft should treat as `deft-directory`"
  (let (old-dir deft-directory)
    (setq deft-directory dir)
    (relaunch-deft)))

(defun relaunch-deft ()
  "Relaunch deft instead of just switching back to it."
  (interactive)
  (when (get-buffer "*Deft*")
    (kill-buffer "*Deft*"))
  (deft))

;; Deft configuration
(global-set-key (kbd "C-$ C-$") '(lambda () (interactive)(launch-deft-in "~/Dropbox/Reference")))
(global-set-key (kbd "C-$ p")   '(lambda () (interactive)(launch-deft-in "~/Dropbox/Perceptron")))
(global-set-key (kbd "C-$ w")   '(lambda () (interactive)(launch-deft-in "~/Dropbox/Reference/Work")))

(setq deft-directory "~/Dropbox/Reference")
(setq deft-use-filename-as-title t)
(setq deft-recursive t)

(provide 'custom-deft)
;;; custom-deft.el ends here
