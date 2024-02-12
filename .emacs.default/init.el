;;; init_emacs --- main_config_of_lukemacs

;;; Commentary:
;; READ THE README

;;; Code:

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

;; Icons
(use-package all-the-icons
  :ensure t)

;; Set up treemacs
(use-package treemacs
  :ensure t
  :init
  (treemacs)
  (treemacs-hide-gitignored-files-mode 1)
  :config
  (add-hook 'treemacs-mode-hook (lambda () (treemacs-resize-icons 17)))
  (setq treemacs-width 30)
  )

;; (use-package simpleclip
;;   :ensure t
;;   :config
;;   (simpleclip-mode 1)
;;   )

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
  :ensure t
  :commands dap-debug
  :config
  (require 'dap-node)
  (dap-node-setup)
  )

;;; LSP configuration
(use-package lsp-mode
  :ensure t
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
  :ensure t
  :after lsp
  )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  )

(defun my-fly-mode ()
  "Allow the use of lsp in tty mode."
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
                      :foreground "purple")
  (set-face-attribute 'flycheck-posframe-info-face
                      nil
                      :foreground "cyan")
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

;; Set up Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

;; Set up Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Nicer Dired mode
(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  )

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

;;; Multiple major modes
(use-package multi-mode
  :ensure t
  )

(use-package tree-sitter
  :ensure t
  )

;;; Load other files
(mapc 'load (file-expand-wildcards "~/.emacs.default/custom/*.el"))

(mapc 'load (file-expand-wildcards "~/.emacs.default/epitech/*.el"))

(mapc 'load (file-expand-wildcards "~/.emacs.default/languages/*.el"))

;;; Quick fixes
;;(setq debug-on-error t)
(add-to-list 'warning-suppress-types '(lsp-mode))

;;; yes or no change
(fset 'yes-or-no-p 'y-or-n-p)

;; No warnings
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-horizon))
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1" "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" "37b6695bae243145fa2dfb41440c204cd22833c25cd1993b0f258905b9e65577" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "32f22d075269daabc5e661299ca9a08716aa8cda7e85310b9625c434041916af" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87" "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a" "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "993aac313027a1d6e70d45b98e121492c1b00a0daa5a8629788ed7d523fe62c1" default))
 '(package-selected-packages
   '(go-mode lsp-ui which-key vterm vscode-dark-plus-theme treemacs-all-the-icons projectile magit lsp-mode flycheck dashboard)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
