;; Copyright (C) 2000 Philip Dorrell

;-----------------------------------------------------------------
(defun show-position ()
  "Show current position"
  (interactive)
  (message "Point= %d, column = %d" (point) (current-column)) )

;-----------------------------------------------------------------
(defun point-to-top ()
  "Move point to top of window"
  (interactive)
  (recenter 0) )

;-----------------------------------------------------------------
(defun point-to-bottom ()
  "Move point to bottom of window"
  (interactive)
  (recenter -1) )

;-----------------------------------------------------------------
(defun scroll-up-one()
  "Scroll up one line"
  (interactive)
  (scroll-up 1) )

(defun scroll-down-one()
  "Scroll down one line"
  (interactive)
  (scroll-down 1) )


(defvar scroll-sideways-increment 30 "Amount to scroll sideways in one go")

(defun scroll-left-a-bit ()
  "Scroll buffer left a bit"
  (interactive)
  (scroll-left scroll-sideways-increment) )

(defun scroll-right-a-bit ()
  "Scroll buffer right a bit"
  (interactive)
  (scroll-right scroll-sideways-increment) )

(setq automatic-hscrolling nil)
