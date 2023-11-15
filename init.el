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

;; Load additional Elisp files
(setq custom-lisp-dir "~/.emacs.default/custom/")
(add-to-list 'load-path custom-lisp-dir)
(mapc 'load (directory-files custom-lisp-dir t "^[^#].*el$"))

;; Set up vterm
(use-package vterm
  :ensure t)

(defun my-open-vterm-in-frame ()
  "Open vterm in a new frame."
  (interactive)
  (make-frame '((name . "VTerm") (width . 100) (height . 30)))
  (vterm "*vterm*"))

(global-set-key (kbd "C-c t") 'my-open-vterm-in-frame)

;; Set up lsp-mode
(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp)
  :commands lsp)

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
  :ensure t
)
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

;; Enable smooth scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; Use the Spacemacs theme
(load-theme 'spacemacs-dark t)

;; End of Emacs configuration
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(spacemacs yasnippet use-package treemacs-magit spacemacs-theme projectile org-plus-contribmaterial-theme helm-xref helm-lsp flycheck dashboard dap-mode company-shell company-c-headers)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; fixes to problems
(setq default-frame-alist '((undecorated . t) (inhibit-double-buffering . t)))
(setq ring-bell-function 'ignore)
