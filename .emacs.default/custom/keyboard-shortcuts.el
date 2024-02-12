;;; Custom_keybinds --- custom keybings for lukemacs

;;; Commentary:

;;; Code:

(global-set-key "\C-n" 'tab-bar-new-tab)
(global-set-key "\C-q" 'tab-bar-close-tab)
(global-set-key "\C-cr" 'replace-string)
(global-set-key "\C-cv" 'vterm-copy-mode)
(global-set-key "\C-ct" 'my-open-vterm)
(global-set-key "\C-xt" 'indent-tabs-mode)

;; Treemacs specific keybinds
(global-set-key "\C-t" 'treemacs-select-window)
(global-set-key "\C-c\C-t" 'treemacs-select-directory)
(global-set-key "\C-c\C-s" 'treemacs-switch-workspace)
(global-set-key "\C-c\C-z" 'treemacs-create-workspace)
(global-set-key "\C-c\C-e" 'treemacs-edit-workspaces)

;; Dired mode keybinds
(global-set-key "\C-c\C-d" 'dirvish)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory))

;; Lsp mode keybinds
(add-hook 'lsp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-<return>") #'lsp-find-definition)))

;; Make ESC quit everything
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun my/open-term-from-tree ()
  (interactive)
  (treemacs-copy-absolute-path-at-point)
  (let ((default-directory (current-kill 0 'DONT-MOVE)))
    (start-process "alacritty-process" nil "alacritty"))
  )



(global-set-key (kbd "C-c y") 'my/open-term-from-tree)


;;; keyboard-shortcuts.el ends here
