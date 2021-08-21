;; Copyright (C) 2000 Philip Dorrell

;-----------------------------------------------------------------
(defun accent-mode()
  "Set options for typing accents"
  (interactive)
  (iso-accents-mode 1)
  (standard-display-european 1)
  (message "Accent typing and display enabled") )

