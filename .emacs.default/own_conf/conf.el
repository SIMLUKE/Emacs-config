;;; perso_conf --- apearence of the tabs

;;; Commentary:

;;; Code:

;; Terminal openers
(defun my/open-term-from-tree ()
  (interactive)
  (treemacs-copy-absolute-path-at-point)
  (let ((default-directory (current-kill 0 'DONT-MOVE)))
    (start-process "gnome-terminal-process" nil "gnome-terminal"))
  )
(global-set-key (kbd "C-c y") 'my/open-term-from-tree)

(defun my/open-term-on-project ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (start-process "gnome-terminal-process" nil "gnome-terminal")))
(global-set-key (kbd "C-c a") 'my/open-term-from-tree)

;;; conf.el ends here
