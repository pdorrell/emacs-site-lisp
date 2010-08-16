;; Copyright (C) 2000 Philip Dorrell

;-----------------------------------------------------------------
(defun show-position ()
  "Show current position"
  (interactive)
  (message "Point= %d, column = %d" (point) (current-column)) )

(global-set-key [?\M-P] 'show-position)

(global-set-key [?\C-\M-g] 'goto-line)

;-----------------------------------------------------------------
(defun point-to-top ()
  "Move point to top of window"
  (interactive)
  (recenter 0) )

(global-set-key [M-S-up] 'point-to-top)
(global-set-key [M-S-kp-up] 'point-to-top)

;-----------------------------------------------------------------
(defun point-to-bottom ()
  "Move point to bottom of window"
  (interactive)
  (recenter -1) )

(global-set-key [M-S-down] 'point-to-bottom)
(global-set-key [M-S-kp-down] 'point-to-bottom)

;-----------------------------------------------------------------
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [C-home] 'beginning-of-buffer)
(global-set-key [C-end] 'end-of-buffer)
(global-set-key [C-kp-home] 'beginning-of-buffer)
(global-set-key [C-kp-end] 'end-of-buffer)

(global-set-key [M-left] 'backward-sexp)
(global-set-key [M-kp-left] 'backward-sexp)
(global-set-key [M-right] 'forward-sexp)
(global-set-key [M-kp-right] 'forward-sexp)

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
  (interactive)
  (scroll-left scroll-sideways-increment) )

(defun scroll-right-a-bit ()
  (interactive)
  (scroll-right scroll-sideways-increment) )

(global-set-key [C-left] 'scroll-left-a-bit)
(global-set-key [C-right] 'scroll-right-a-bit)
(global-set-key [C-kp-left] 'scroll-left-a-bit)
(global-set-key [C-kp-right] 'scroll-right-a-bit)
(setq automatic-hscrolling nil)

(global-set-key [C-M-up] 'scroll-up-one)
(global-set-key [C-M-kp-up] 'scroll-up-one)
(global-set-key [C-M-down] 'scroll-down-one)
(global-set-key [C-M-kp-down] 'scroll-down-one)
