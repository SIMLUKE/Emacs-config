;;; yaml_conf --- Custom yaml completion for lukemacs

;;; Commentary:

;;; Code:

(use-package yaml-mode
  :ensure t
  )

(add-hook 'yaml-mode-hook 'lsp)

;;; yaml_conf.el ends here
