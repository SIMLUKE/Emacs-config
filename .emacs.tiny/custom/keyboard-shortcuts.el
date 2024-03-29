;;; Custom_keybinds --- custom keybings for lukemacs

;;; Commentary:

;;; Code:

(global-set-key "\C-n" 'tab-bar-new-tab)
(global-set-key "\C-q" 'tab-bar-close-tab)
(global-set-key "\C-cr" 'replace-string)
(global-set-key "\C-cp" 'company-mode)
(global-set-key "\C-cv" 'vterm-copy-mode)
(global-set-key "\C-ct" 'my-open-vterm)
(global-set-key "\C-xs" 'switch-to-buffer-other-frame)
(global-set-key "\C-xt" 'indent-tabs-mode)

;; Make ESC quit everything
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Dired mode keybinds
(global-set-key "\C-cd" 'dirvish)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory))

;;; keyboard-shortcuts.el ends here
