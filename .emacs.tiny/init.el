;;; init -- init for tiny of lukemacs

;;; Commentary:

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

;; Company mode
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)

;; Enable company-box for a better UI
  (use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode)))
(global-company-mode)

;; Set up which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Set up ido
(use-package ido
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-create-new-buffer 'always))

;;; Backup handling
(setq backup-directory-alist `(("." . , "~/.emacs.tiny/backups")))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;;; yes or no change
(fset 'yes-or-no-p 'y-or-n-p)

;;; Load other files
(mapc 'load (file-expand-wildcards "~/.emacs.default/custom/*.el"))

(mapc 'load (file-expand-wildcards "~/.emacs.tiny/epitech/*.el"))

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
 '(custom-enabled-themes '(doom-material-dark))
 '(custom-safe-themes
   '("da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages '(doom-modeline which-key vscode-dark-plus-theme))
 '(tabbar-separator '(0.5)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
