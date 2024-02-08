;;; c_conf --- custom c conf for lukemacs

;;; Commentary:

;;; Code:

(add-hook 'c-mode-hook 'lsp)
(setq lsp-clients-clangd-executable "/usr/bin/clangd")

(add-hook 'asm-mode-hook 'lsp)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(asm-mode . "asm")))
;;; c.el ends here
