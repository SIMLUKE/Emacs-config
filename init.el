
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
(require 'bind-key)

(setq-default c-basic-offset 4
              indent-tabs-mode nil
              tab-width 4)

(defun my-c-mode-hook ()
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

;; theme
(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))
(setq vscode-dark-plus-box-org-todo nil)
(setq vscode-dark-plus-scale-org-faces nil)
(setq vscode-dark-plus-invert-hl-todo nil)
(setq vscode-dark-plus-render-line-highlight 'line)

;; Set up vterm
(use-package vterm
  :ensure t)

(defun my-open-vterm ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (vterm (generate-new-buffer-name "vterm")))
(global-set-key (kbd "C-c t") 'my-open-vterm)

;; Set up lsp-mode
(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp)
  :commands lsp
  :init
  (setq lsp-clients-clangd-executable "/usr/bin/clangd")
  :config
  )
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

;; Set up Treemacs
(use-package treemacs
  :ensure t)
(treemacs)
(with-eval-after-load 'treemacs
  (defun treemacs-ignore-gitignore (file _)
    (string= file ".gitignore"))
  (push #'treemacs-ignore-gitignore treemacs-ignored-file-predicates))

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

;; Set up Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Enable cool scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; End of Emacs configuration
(custom-set-variables
 '(package-selected-packages
   '(spacemacs yasnippet use-package treemacs-magit spacemacs-theme projectile org-plus-contribmaterial-theme helm-xref helm-lsp flycheck dashboard dap-mode company-shell company-c-headers)))
(custom-set-faces
 )

;; fixes to problems
(setq default-frame-alist '((undecorated . t) (inhibit-double-buffering . t)))
(setq ring-bell-function 'ignore)
(add-hook 'emacs-startup-hook (lambda ()
                                (when (get-buffer "*scratch*")
                                  (kill-buffer "*scratch*"))))

;; Load additional lisp files
(setq custom-lisp-dir "~/.emacs.default/custom/")
(add-to-list 'load-path custom-lisp-dir)
(mapc 'load (file-expand-wildcards (concat custom-lisp-dir "*.el")))

