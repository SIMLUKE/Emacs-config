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

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)

  (doom-themes-visual-bell-config)

  ;; Uncomment this for minimal icons
  ;; (setq doom-themes-treemacs-theme "doom-colors")
  ;; (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (display-time-mode 1)
  )

;; sets up the columns numbers
(setq column-number-mode t)

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; cleaner emacs
(tab-bar-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)

(use-package windresize
  :ensure t
  )

(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)
;; funny rainbow
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; interface.el ends here
