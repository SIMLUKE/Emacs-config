;; Define package archives
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Initialize packages
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;; Theme
(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))
(setq vscode-dark-plus-box-org-todo nil)
(setq vscode-dark-plus-scale-org-faces nil)
(setq vscode-dark-plus-invert-hl-todo nil)
(setq vscode-dark-plus-render-line-highlight 'line)

;; Icons
(use-package all-the-icons
  :ensure t)

;; Fat treemacs config
(use-package treemacs
  :ensure t
  :init
  (treemacs)
  (treemacs-hide-gitignored-files-mode)
  )

(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs
  )

;; Set up vterm
(use-package vterm
  :ensure t)

(defun my-open-vterm ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (vterm (generate-new-buffer-name "vterm")))
(global-set-key (kbd "C-c t") 'my-open-vterm)

(defun my/projectile-run-alacritty-in-root ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (start-process "alacritty-process" nil "alacritty")))
(global-set-key (kbd "C-c a") 'my/projectile-run-alacritty-in-root)

;;; Debugger
(use-package dap-mode
  :commands dap-debug
  :config
  (require 'dap-node)
  (dap-node-setup)
  )

;;; Set up lsp-mode
;; Set the path to clangd (update with your actual path)
(setq lsp-clients-clangd-executable "/usr/bin/clangd")

;; LSP configuration
(use-package lsp-mode
  :ensure t
  :hook ((c-mode c++-mode) . lsp)
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet t)
  (setq lsp-idle-delay 0.5)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-segments '(file symbols))
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-signature-auto-activate nil)
  )

;; LSP UI tools
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-alignment 'frame)
  (setq lsp-ui-doc-delay 2)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-doc-show-with-cursor t)
  )

(use-package lsp-treemacs
  :after lsp)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  )

(defun my-fly-mode ()
  (interactive)
  (if (window-system)
        (progn
          (flycheck-posframe-mode))
      )
    )

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config
  (add-hook 'find-file-hook #'my-fly-mode)
  (setq flycheck-posframe-warning-prefix " ")
  (setq flycheck-posframe-error-prefix " ")
  (set-face-attribute 'flycheck-posframe-error-face
                      nil
                      :inherit nil
                      :foreground "red")
  (set-face-attribute 'flycheck-posframe-warning-face
                      nil
                      :foreground "orange")
  (set-face-attribute 'flycheck-posframe-info-face
                      nil
                      :foreground "blue")
  (set-face-attribute 'flycheck-posframe-border-face
                      nil
                      :foreground "#555555")
  (setq flycheck-posframe-border-width 2)
)

;; Company mode for auto-completion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  (use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode)))
(global-company-mode)

;; Set up which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Set up dashboard
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-set-navigator t))

;; Set up ido
(use-package ido
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-create-new-buffer 'always))

;; Set up whitespace deletion
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set up Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

;; Set up Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;;; Load other files
(setq custom-lisp-dir "~/.emacs.default/custom/")
(add-to-list 'load-path custom-lisp-dir)
(mapc 'load (file-expand-wildcards (concat custom-lisp-dir "*.el")))

(setq custom-lisp-dir "~/.emacs.default/epitech/")
(add-to-list 'load-path custom-lisp-dir)
(mapc 'load (file-expand-wildcards (concat custom-lisp-dir "*.el")))

;;; Quick fixes
;;(setq debug-on-error t)

;; No warnings
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("993aac313027a1d6e70d45b98e121492c1b00a0daa5a8629788ed7d523fe62c1" default))
 '(package-selected-packages
   '(lsp-ui which-key vterm vscode-dark-plus-theme treemacs-all-the-icons projectile magit lsp-mode flycheck dashboard)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(treemacs-directory-face ((t (:foreground "LightSkyBlue1"))))
 '(treemacs-file-face ((t (:foreground "white"))))
 '(treemacs-root-face ((t (:foreground "gold" :weight bold)))))
