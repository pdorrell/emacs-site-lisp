;; Copyright (C) 2000 Philip Dorrell

;;========================================================================
(defun make-alpha-table (&rest strings)
  (let ( (table (make-char-table 'word-table nil)) )
    (dolist (string strings)
      (let ((len (length string)))
	(dotimes (i len)
          (set-char-table-range table (aref string i) t) ) ) )
    table) )

(defun make-inverse-alpha-table (&rest strings)
  (let ( (table (make-char-table 'word-table t)) )
    (dolist (string strings)
      (let ((len (length string)))
	(dotimes (i len)
          (let ((ch (aref string i)))
            (set-char-table-range table ch 'false) ) ) ) )
    table) )

(defvar letters-digits-string 
  (concat "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	  "abcdefghijklmnopqrstuvwxyz"
	  "1234567890") )

(make-variable-buffer-local 'word-alpha-table)
(set-default 'word-alpha-table 
	     (make-alpha-table letters-digits-string "_"))

(defun buffer-char-in-table (table pos)
  (let ( (ch (char-after pos)) )
    (if ch
	(eq (char-table-range table ch) t)
      nil) ) )

(defun word-pos (table pos)
  (let ((start pos) (end pos) length)
    (while (buffer-char-in-table table start)
      (setq start (1- start)) )
    (setq start (1+ start))
    (while (buffer-char-in-table table end)
      (setq end (1+ end))) 
    (setq length (- end start))
    (if (> end start)
	(cons start end)
      nil) ) )

(defun word-at (table pos)
  (let ( (word-pos (word-pos table pos)) )
    (if word-pos (buffer-substring-no-properties 
		  (car word-pos) (cdr word-pos) )
      nil) ) )

(defun word-before (table pos)
  (let ((start (1- pos)) (end pos) length)
    (while (buffer-char-in-table table start)
      (setq start (1- start)) )
    (setq start (1+ start))
    (setq length (- end start))
    (if (> end start)
	(buffer-substring-no-properties start end)
      nil) ) )

(defun word-clicked-on (event &optional table)
    (let* ( (pos (second (second event)))
	 (window (first (second event)))
	 (buffer (window-buffer window))
	 (word-table (if table table word-alpha-table))
	 word)
    (if (eq buffer (current-buffer))
	(setq word (word-at word-table pos))
      (save-window-excursion
	(switch-to-buffer buffer)
	(setq word (word-at word-table pos)) ) )
    word) )

(defvar last-paste-start 0)
(defvar last-paste-end 0)

(defun paste-word (event)
  "paste word"
  (interactive "e")
  (let ( (word (word-clicked-on event)) )
    (if word
	(progn
	  (setq last-paste-start (point))
	  (insert word)
	  (setq last-paste-end (point)) )
      (message "No word at mouse position") ) ) )

(setq line-alpha-table (make-inverse-alpha-table "\n"))

(defun paste-line (event)
  "copy clicked on line"
  (interactive "e")
  (let ( (line (word-clicked-on event line-alpha-table)) )
    (when line
	(beginning-of-line)
	(insert line "\n") ) ) )

(defun delete-word-at-point()
  "delete word at point"
  (interactive)
  (let ( (word-pos (word-pos word-alpha-table (point))) )
    (if word-pos
	(delete-region (car word-pos) (cdr word-pos)) ) ) )


(defun replace-word (event)
  "paste word"
  (interactive "e")
  (let ( (word (word-clicked-on event)) )
    (if word
      (progn (delete-word-at-point)
	     (insert word) )
      (message "No word at mouse position") ) ) )
