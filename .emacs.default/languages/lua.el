;;; lua_conf --- custom lua conf for lukemacs

;;; Commentary:

;;; Code:

(use-package company-lua
  :ensure t
  )

(add-hook 'lua-mode-hook 'lsp)

;;; lua.el ends here
