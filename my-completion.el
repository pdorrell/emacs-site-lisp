;; Copyright (C) 2001 Philip Dorrell

(defvar saved-completing-buffer nil "Global variable to hold pointer to buffer being completed")
(defvar saved-completion-insertion-function nil "Global variable to hold pointer to insertion function for completion")

;; Note 'completion' could actually be anything that can process a line selected
;; by the user, since 'insertion-function' could be anything that can be called with the line
;; TODO - more general naming
(defun show-completion-buffer-with-string (name string insertion-function)
  "Show completion buffer NAME with content STRING, calling INSERTION-FUNCTION
   on the line selected by the user"
  (setq saved-completing-buffer (current-buffer))
  (setq saved-completion-insertion-function insertion-function)
  (switch-to-buffer name)
  (clear-buffer)
  (insert string)
  (line-completion-mode) )


(defvar line-completion-mode-map nil "Keymap for line-completion-mode")
(if line-completion-mode-map
    nil
  (setq line-completion-mode-map (make-sparse-keymap))
  (define-key line-completion-mode-map "\C-m" 'line-completion-accept-line) )

(defun line-completion-accept-line ()
  "Accept current line as completion"
  (interactive)
  (let ( line-start line-end line the-completing-buffer the-completion-function )
    (beginning-of-line)
    (setq line-start (point))
    (end-of-line)
    (setq line-end (point))
    (setq line (buffer-substring line-start line-end))
    (setq the-completing-buffer completing-buffer)
    (setq the-completion-function completion-function)
    (switch-to-buffer the-completing-buffer)
    (funcall the-completion-function line) ) )

(defun mouse-line-completion-accept-line (event)
  "Process mouse click by accepting completion from the line that the mouse clicked on"
  (interactive "e")
  (let* ( (pos (second (second event))) )
    (goto-char pos)
    (line-completion-accept-line) ) )

(defun line-completion-mode ()  ;; todo - pass in completing-buffer completion-function as local variables?
  "A mode for a buffer which is effectively a dialog presenting lines of text 
   where the user can select one line of text to set a 'completion' in the calling buffer"
  (setq major-mode 'line-completion-mode
	mode-name "Line Completion" )
  (use-local-map line-completion-mode-map)
  (local-set-key [mouse-3] 'mouse-line-completion-accept-line)
  (local-set-key [?\M-K] 'bury-buffer)
  (make-local-variable 'completing-buffer)
  (setq completing-buffer saved-completing-buffer)
  (make-local-variable 'completion-function)
  (setq completion-function saved-completion-insertion-function) )
