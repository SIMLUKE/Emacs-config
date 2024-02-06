;;; c_conf --- custom c conf for lukemacs

;;; Commentary:

;;; Code:

(add-hook 'c-mode-hook 'lsp)
(setq lsp-clients-clangd-executable "/usr/bin/clangd")

;;; c.el ends here
