;; Copyright (C) 2001 Philip Dorrell

(defvar saved-completing-buffer nil "Global variable to hold pointer to buffer being completed")
(defvar saved-completion-insertion-function nil "Global variable to hold pointer to insertion function for completion")

(defun show-completion-buffer-with-string (name string insertion-function)
  "Show completion buffer NAME with content STRING, calling INSERTION-FUNCTION
   on the line selected by the user"
  (setq saved-completing-buffer (current-buffer))
  (setq saved-completion-insertion-function insertion-function)
  (switch-to-buffer name)
  (clear-buffer)
  (insert string)
  (line-completion-mode) )

(setq line-completion-first-line-regexp 
      (make-regexp '(seq start (at-least-once (not-set " \t")))) )

(defvar line-completion-mode-map nil)
(if line-completion-mode-map
    nil
  (setq line-completion-mode-map (make-sparse-keymap))
  (define-key line-completion-mode-map [C-pause] 'line-completion-kill-buffer)
  (define-key line-completion-mode-map "\C-m" 'line-completion-accept-line) )

(defun line-completion-kill-buffer ()
  (interactive)
  (if completion-process
      (process-send-string completion-process "exit#\n") )
  (kill-buffer nil)
  (message "Killed completion buffer") )

(setq line-completion-comment-start-regexp
      (make-regexp '(or end (paren (seq (repeated (set " \t")) "!")))) )

(defun line-completion-accept-line ()
  (interactive)
  "Accept current line as completion"
  (let ( line-start line-end line the-completing-buffer the-completion-function )
    (beginning-of-line)
    (setq line-start (point))
    (search-forward-regexp line-completion-comment-start-regexp)
    (setq line-end (match-beginning 0))
    (setq line (buffer-substring line-start line-end))
    (setq the-completing-buffer completing-buffer)
    (setq the-completion-function completion-function)
    (switch-to-buffer the-completing-buffer)
    (funcall the-completion-function line) ) )

(defun mouse-line-completion-accept-line (event)
  (interactive "e")
  (let* ( (pos (second (second event))) )
    (goto-char pos)
    (line-completion-accept-line) ) )

(defun line-completion-mode ()
  (setq major-mode 'line-completion-mode
	mode-name "Line Completion" )
  (use-local-map line-completion-mode-map)
  (local-set-key [mouse-3] 'mouse-line-completion-accept-line)
  (local-set-key [?\M-K] 'bury-buffer)
  (make-local-variable 'completing-buffer)
  (setq completing-buffer saved-completing-buffer)
  (make-local-variable 'completion-function)
  (make-local-variable 'completion-process)
  (setq completion-function saved-completion-insertion-function) )


;;-----------------------------------------------------------------
(setq parameter-open-bracket-regexp (make-regexp "#("))
(setq parameter-close-bracket-regexp (make-regexp ")# "))

(defun goto-first-parameter-on-line ()
  (let ( pos )
    (save-excursion
      (beginning-of-line)
      (and
       (search-forward-regexp parameter-open-bracket-regexp nil t)
       (search-forward-regexp parameter-close-bracket-regexp)
       (setq pos (point)) ) )
    (if pos
	(goto-char pos) ) ) )

(defun delete-previous-parameter ()
  (let ( (here (point))
	 line-start line-end param-start param-end deleted-previous)
    (save-excursion (beginning-of-line) (setq line-start (point)))
    (save-excursion
      (and
       (search-backward-regexp parameter-open-bracket-regexp line-start t)
       (setq param-start (match-beginning 0))
       (search-forward-regexp parameter-close-bracket-regexp here t)
       (setq param-end (match-end 0))
       (progn 
	 (delete-region param-start param-end)
	 (setq deleted-previous t) ) ) )
    (message "deleted-previous=%s" deleted-previous)
    deleted-previous) )

(defun goto-next-parameter (on-this-line)
  (let ( bound line-end param-pos )
    (message "on-this-line=%s" on-this-line)
   (if on-this-line
       (save-excursion (end-of-line) (setq bound (point))) )
   (save-excursion
     (and
      (message "bound=%s" bound)
      (search-forward-regexp parameter-open-bracket-regexp bound t)
      (save-excursion (end-of-line) (setq line-end (point)))
      (search-forward-regexp parameter-close-bracket-regexp line-end t)
      (setq param-pos (match-end 0)) ) )
   (if param-pos (goto-char param-pos))
   param-pos) )
     

(defun next-parameter ()
  (interactive)
  (let* ( (previous-deleted (delete-previous-parameter))
	  (went-to-next (goto-next-parameter previous-deleted)) )
    (if previous-deleted
	(if (not went-to-next)
	    (progn
	      (end-of-line)
	      (message "No more parameters on line") ) )
      (if (not went-to-next)
	  (message "No more parameters in file") ) ) ) )

