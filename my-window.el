;; Copyright (C) 2000 Philip Dorrell

;;========================================================================
(defun window-only-and-split ()
  "Delete other windows and split this one"
  (interactive)
  (delete-other-windows)
  (split-window-vertically) )

;;========================================================================
(global-set-key "\M-0" 'delete-other-windows)
(global-set-key "\M-9" 'split-window-vertically)
(global-set-key "\M-8" 'window-only-and-split)
(global-set-key "\M-7" 'delete-window)

