;; Copyright (C) 2000,2001 Philip Dorrell

;;========================================================================
(defun set-debug-on ()
  (interactive)
  (setq debug-on-error t)
  (message "Set debug on") )

(defun set-debug-off ()
  (interactive)
  (setq debug-on-error nil) 
  (message "Set debug off") )


;;========================================================================
(global-set-key [f9] 'eval-defun)
(global-set-key [S-f9] 'eval-region)

(global-set-key [?\M-Z] 'set-debug-on)
(global-set-key [?\M-Y] 'set-debug-off)
