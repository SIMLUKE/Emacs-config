;;; Custom_keybinds --- custom keybings for lukemacs

;;; Commentary:

;;; Code:

(global-set-key "\C-n" 'tab-bar-new-tab)
(global-set-key "\C-q" 'tab-bar-close-tab)
(global-set-key "\C-cr" 'replace-string)

(global-set-key "\C-cv" 'vterm-copy-mode)
(global-set-key "\C-xt" 'vterm-copy-mode-done)

(global-set-key (kbd "C-)") 'undo)
(global-set-key (kbd "C-c C-r") 'projectile-run-project)
(global-set-key (kbd "C-c RET") 'projectile-compile-project)

;; Move between windows
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

;; resize
(global-set-key (kbd "C-c m") 'windresize)

;; Treemacs specific keybinds
(global-set-key "\C-t" 'treemacs-select-window)
(global-set-key "\C-c\C-t" 'treemacs-select-directory)
(global-set-key "\C-c\C-s" 'treemacs-switch-workspace)
(global-set-key "\C-c\C-z" 'treemacs-create-workspace)
(global-set-key "\C-c\C-e" 'treemacs-edit-workspaces)
(define-key treemacs-mode-map [drag-mouse-1] nil)

;; Dired mode keybinds
(global-set-key "\C-cd" 'dirvish)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory))

;; Lsp mode keybinds
(add-hook 'lsp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-<return>") #'lsp-find-definition)))

;; Make ESC quit everything
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun my-open-vterm-hor ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (vterm (generate-new-buffer-name "vterm")))
(global-set-key (kbd "C-c 3") 'my-open-vterm-hor)

(defun my-open-vterm-ver ()
  (interactive)
  (split-window-below)
  (windmove-down)
  (vterm (generate-new-buffer-name "vterm")))
(global-set-key (kbd "C-c 2") 'my-open-vterm-ver)

(global-set-key (kbd "C-c f") 'mini-frame-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; keyboard-shortcuts.el ends here
