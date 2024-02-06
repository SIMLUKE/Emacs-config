;;; Programing_conf --- everything generally related to programing mode

;;; Commentary:

;;; Code:

;; nukes trailling spaces
(add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)
(defun my-prog-nuke-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

;; auto completes pairs
(electric-pair-mode 1)

;;; programing.el ends here
