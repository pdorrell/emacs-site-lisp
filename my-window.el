;; Copyright (C) 2000 Philip Dorrell

;;========================================================================
(defun window-only-and-split ()
  "Delete other windows and split this one"
  (interactive)
  (delete-other-windows)
  (split-window-vertically) )
