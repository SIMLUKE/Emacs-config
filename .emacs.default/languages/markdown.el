;;; markdown --- Custom typescript completion for lukemacs

;;; Commentary:

;;; Code:
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "ghostwriter")
  )

;;; markdown.el ends here
