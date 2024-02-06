;;; Custom_interface --- Anything appearence related

;;; Commentary:

;;; Code:

;; Themes (feel free to add more or replace)
(use-package vscode-dark-plus-theme
  :ensure t
  )
(setq vscode-dark-plus-box-org-todo nil)
(setq vscode-dark-plus-scale-org-faces nil)
(setq vscode-dark-plus-invert-hl-todo nil)
(setq vscode-dark-plus-render-line-highlight 'line)

;; Loads theme (here to replace with your own)
(load-theme 'vscode-dark-plus t)

;; sets up the columns numbers
(setq column-number-mode t)

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; cleaner emacs
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; funny rainbow
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; interface.el ends here
