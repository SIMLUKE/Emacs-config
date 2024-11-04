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

(mapc 'load (file-expand-wildcards "~/.emacs.default/own_conf/*.el"))

;; Icons
(use-package all-the-icons
  :ensure t)

(use-package treemacs
  :ensure t
  :init
  (treemacs)
  :config
  (add-hook 'treemacs-mode-hook (lambda () (treemacs-resize-icons 17)))
  (setq treemacs-width 30)
  )

(treemacs-hide-gitignored-files-mode 1)
(treemacs-indent-guide-mode 1)

;; Send kill ring to sys clippboard
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode)
  )

(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs
  )

;; Set up vterm
(use-package vterm
  :ensure t)

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
        company-idle-delay 0.0
        )
  :bind ("C-)" . company-abort))
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))
(global-company-mode)

(use-package yasnippet
  :ensure t
  )
(yas-global-mode)

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

(add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'recency)
        (ibuffer-do-sort-by-recency))))


;; Set up Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  )

;; Set up Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  )

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

(use-package all-the-icons-ibuffer
  :ensure t
  :config
  (add-hook 'ibuffer-mode-hook 'all-the-icons-ibuffer-mode)
  )

(use-package ibuffer-projectile
  :ensure t
  )

(use-package mini-frame
  :ensure t
  :config
  (setq mini-frame-standalone t)
  (setq mini-frame--fit-frame-function t)
  )

;;; Load other files
(mapc 'load (file-expand-wildcards "~/.emacs.default/custom/*.el"))

(mapc 'load (file-expand-wildcards "~/.emacs.default/epitech/*.el"))

(mapc 'load (file-expand-wildcards "~/.emacs.default/languages/*.el"))

;;; Quick fixes
;;(setq debug-on-error t)

;;; yes or no change
(fset 'yes-or-no-p 'y-or-n-p)

;;; Backup handling
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.
;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.default/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.default/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

;; No warnings
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-xcode))
 '(custom-safe-themes
   '("af077a05e630a9c4e92ada984cdd7eeaf4e8b12185e8f011821576b24014fa5a" "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d" "6e33d3dd48bc8ed38fd501e84067d3c74dfabbfc6d345a92e24f39473096da3f" "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc" "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "e8ceeba381ba723b59a9abc4961f41583112fc7dc0e886d9fc36fa1dc37b4079" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2" "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "c8b3d9364302b16318e0f231981e94cbe4806cb5cde5732c3e5c3e05e1472434" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434" "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b" "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1" "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" "37b6695bae243145fa2dfb41440c204cd22833c25cd1993b0f258905b9e65577" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "32f22d075269daabc5e661299ca9a08716aa8cda7e85310b9625c434041916af" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87" "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a" "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "993aac313027a1d6e70d45b98e121492c1b00a0daa5a8629788ed7d523fe62c1" default))
 '(mini-frame-show-parameters '((top . 10) (width . 0.7) (left . 0.5)))
 '(package-selected-packages
   '(eslint-fix multiple-cursors go-snippets biomejs-format company-lua prettier js-react-redux-yasnippets all-the-icons-ibuffer ibuffer-projectile rust-mode system-packages treemacs-magit 0x0 2048-game go-mode lsp-ui which-key vterm vscode-dark-plus-theme treemacs-all-the-icons projectile magit lsp-mode flycheck dashboard))
 '(tabbar-separator '(0.5)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-black ((t (:background "gray20" :foreground "gray45"))))
 '(ansi-color-bright-black ((t (:background "gray20" :foreground "gray35"))))
 '(hl-line ((t (:extend t :background "dim gray"))))
 '(region ((t (:extend t :background "dim gray"))))
 '(tab-bar ((t (:background "gray20" :foreground "gray85" :box (:line-width (3 . 3) :color "gray30" :style released-button)))))
 '(tab-bar-tab ((t (:background "gray15" :foreground "gray85" :box (:line-width (3 . 3) :color "gray30" :style released-button)))))
 '(tab-bar-tab-inactive ((t (:background "gray20" :foreground "gray85" :box (:line-width (3 . 3) :color "gray30" :style released-button))))))

(require 'org-indent)
(add-hook 'org-mode-hook 'org-indent-mode)


;;; init.el ends here
