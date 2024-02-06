;;; Mouse_conf --- everything generally related to programing mode

;;; Commentary:

;;; Code:

;; Nicer scroller experience
(setq scroll-margin 5)

(unless window-system
  (xterm-mouse-mode)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)

;; customizing the cursor
(setq-default cursor-type 'bar)

;;; mouse.el ends here
