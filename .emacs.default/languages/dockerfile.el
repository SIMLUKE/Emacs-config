;;; docker_conf --- Custom docker completion for lukemacs

;;; Commentary:

;;; Code:

(use-package dockerfile-mode
  :ensure t
  )

(add-hook 'dockerfile-mode-hook 'lsp)

;;; dockerfile.el ends here
