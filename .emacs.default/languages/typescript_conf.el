;;; typescript_conf --- Custom typescript completion for lukemacs

;;; Commentary:

;;; Code:

(use-package typescript-mode
  :ensure t
  )

(add-hook 'typescript-mode-hook 'lsp)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;;; typescript_conf.el ends here
