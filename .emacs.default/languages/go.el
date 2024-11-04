;;; Go_mode --- customized

;;; Commentary:

;;; Code:

(add-hook 'go-mode-hook 'lsp)
(setq exec-path (append exec-path '("~/go/bin")))

(use-package go-mode
  :ensure t
  )

(use-package company-go
  :ensure t
  )

(require 'company)
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)))

(use-package company-go
  :ensure t
  :after (company go-mode)
  :config
  (add-to-list 'company-backends 'company-go))

(use-package go-mode
  :ensure t
  :hook (go-mode . (lambda ()
                     (add-hook 'before-save-hook 'gofmt-before-save nil t))))

(use-package go-snippets
  :ensure t
  )
(defun my-go-hook()
  "Custom go mode hook."
  (setq tab-width 4)
  )
(add-hook 'go-mode-hook 'my-go-hook)

;;; go.el ends here
