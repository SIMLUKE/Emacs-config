;;; debug_conf --- custom c conf for lukemacs

;;; Commentary:

;;; Code:

;;; Debugger
(use-package dap-mode
  :ensure t
  :commands dap-debug
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1)
  )

(setq dap-auto-configure-features '(sessions locals controls tooltip))


;;; debug.el ends here
