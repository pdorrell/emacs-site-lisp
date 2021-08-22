;; Copyright (C) 2000,2001 Philip Dorrell

;;========================================================================
(defun toggle-debug ()
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "Set debug %s" (if debug-on-error "on" "off")) )

;;========================================================================
(global-set-key [f9] 'eval-defun)
(global-set-key [S-f9] 'eval-region)

(global-set-key [C-f12] 'toggle-debug)
