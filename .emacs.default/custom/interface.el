
;; sets up the columns numbers
(setq column-number-mode t)

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
