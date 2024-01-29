
;; sets up the columns numbers
(setq column-number-mode t)

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; cleaner emacs
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; funny rainbow
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
