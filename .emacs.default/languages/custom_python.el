;;; Python_conf --- Custom python ide for lukemacs

;;; Commentary:

;;; Code:

(add-hook 'python-mode-hook 'lsp)

(use-package lsp-pyright
  :ensure t
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))

;;; custom_python.el ends here
