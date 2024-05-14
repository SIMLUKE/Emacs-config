;;; c_conf --- custom c conf for lukemacs

;;; Commentary:

;;; Code:

(add-hook 'c-mode-hook 'lsp)
(setq lsp-clients-clangd-executable "/usr/bin/clangd")

(add-hook 'asm-mode-hook 'lsp)

(add-hook 'c++-mode-hook 'lsp)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(asm-mode . "asm")))

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-clang))

(defun company-c-hook ()
  (setq company-backend 'company-clang)
  )
(add-hook 'c-mode-hook 'company-c-hook)
;;; c.el ends here
