;; Copyright (C) 2008 Philip Dorrell

(defun process-kill-without-query(process)
  (set-process-query-on-exit-flag process nil) )

(defun start-process-if-not-going (name startup-command &optional move-to-top)
  (let ( (process (get-buffer-process (current-buffer))) )
    (if process
	(if (not (eq (process-status process) 'run))
	    (progn 
	      (kill-process process) 
	      (setq process nil)) ) )
    (if (not process)
	(setq process 
	      (apply 'start-process 
		     (append (list name (current-buffer)) startup-command) ) ) )
    (process-kill-without-query process)
    (if move-to-top (set-process-filter process (make-goto-first-line-process-filter)))
    process) )
 
(defun run-process-with-line (name startup-command input-line &optional move-to-top)
  "Start process in current buffer with name NAME that reads input lines if not already started,
   clear any existing output in the buffer, pass in INPUT-LINE. If MOVE-TO-TOP then move to top of output"
  (let ( (process (start-process-if-not-going name startup-command move-to-top)) )
    (clear-buffer)
    (process-send-string process (concat input-line "\n"))
    process) )

(defun run-process-with-lines (name startup-command input-lines &optional move-to-top clear-buffer)
  "Start process in current buffer with name NAME that reads input lines if not already started,
   pass in INPUT-LINES. If MOVE-TO-TOP then move to top of output. If CLEAR-BUFFER then clear
   any existing output in the buffer."
  (let ( (process (start-process-if-not-going name startup-command move-to-top)) )
    (if clear-buffer (clear-buffer))
    (dolist (input-line input-lines)
      (process-send-string process (concat input-line "\n")) )
    process) )

(defconst line-starting-with-non-whitespace-regex 
  (make-regexp-new-and-old '(seq "^" some-non-whitespace) '(seq start (at-least-once (not-set " \t"))))
  "A regex matching start of a line that begins with a non-whitespace character")

(defun goto-first-unindented-line ()
  "Go to first line that doesn't start with white space (the assumption is that non-actionable 
   header lines start with whitespace and you want to sit on the first actionable output line)"
  (goto-char (point-min))
  (if (search-forward-regexp line-starting-with-non-whitespace-regex nil t)
      (progn (beginning-of-line) t)
    nil) )

(defun do-not-move-point-process-filter (process string)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert string) ) )

(defun make-goto-first-line-process-filter ()
  (lexical-let ( (first-time t) )
    (lambda (process string)
      (if first-time
          (let ( (old-buffer (current-buffer))
	         (buffer (process-buffer process)) )
            (when buffer
              (insert string)
              (if (goto-first-unindented-line)
                  (setq first-time nil) ) )
            (set-buffer old-buffer) )
        (do-not-move-point-process-filter process string) ) ) ) )

(defun write-end-of-buffer-sentinel (process event)
  (let ( (buffer (process-buffer process)) )
    (if (and buffer (buffer-live-p buffer))
        (save-excursion
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert (concat "\nProcess " 
                            (process-name process) " " event)) ) ) ) ) )

(defun stop-start-process (name process-variable process-buffer-name
				executable args &optional move-to-top)
  (save-this-buffer-and-others)
  (let ( (process (symbol-value process-variable)) )
    (if (not (equal process-buffer-name (buffer-name (current-buffer))))
	(progn
	  (switch-to-buffer-other-window process-buffer-name) ) )
    (if (and process (not (memq (process-status process) '(exit signal))))
	(progn
	  (delete-process process)
	  (set process-variable nil)
	  (message "%s STOPPED" name) )
      (progn
	(clear-buffer)
	(message "Starting new process %s %s" executable args)
	(let ( (new-process (apply #'start-process name process-buffer-name executable args)) )
	  (set process-variable new-process)
	  (process-kill-without-query new-process)
	  (message "%s STARTED" name)
          (if move-to-top 
              (set-process-filter new-process 'do-not-move-point-process-filter) ) ) ) ) ) )

(defun stop-then-start-process (name process-variable process-buffer-name
				     executable args &optional move-to-top)
  (save-this-buffer-and-others)
  (let ( (process (symbol-value process-variable)) )
    (if (not (equal process-buffer-name (buffer-name (current-buffer))))
	(progn
	  (switch-to-buffer-other-window process-buffer-name) ) )
    (if (and process (not (memq (process-status process) '(exit signal))))
	(progn
	  (delete-process process)
	  (set process-variable nil)
	  (message "%s STOPPED" name) ) )
    (clear-buffer)
    (let ( (new-process (make-process :name name 
                                      :buffer process-buffer-name
                                      :command (cons executable args)
                                      :noquery t
                                      :filter (if move-to-top (make-goto-first-line-process-filter))
                                      :sentinel #'write-end-of-buffer-sentinel) ) )
      (set process-variable new-process)
      (message "%s STARTED" name) ) ) )

(defun start-process-showing-console (name process-buffer-name
				     executable &rest args)
  "Start a server process, where any stopping of existing processes is handled by the script"
  (save-this-buffer-and-others)
  (if (not (equal process-buffer-name (buffer-name (current-buffer))))
      (progn
	(switch-to-buffer-other-window process-buffer-name) ) )
  (clear-buffer)
  (let ( (new-process (apply #'start-process name process-buffer-name executable args)) )
    (set-process-query-on-exit-flag new-process nil) ) )

(defvar *related-output-process* nil 
  "A process associated with a buffer that will be killed by kill-buffer-process if the current buffer has no process")
(make-variable-buffer-local '*related-output-process*)

(defun kill-buffer-process()
  "Kill any process running in the current buffer, or, if there isn't any, the buffer local value of *related-output-process*"
  (interactive)
  (let ( (process-to-kill (get-buffer-process (current-buffer))) )
    (if (null process-to-kill)
        (if *related-output-process*
            (setq process-to-kill *related-output-process*)))
    (if (null process-to-kill)
        (message "No buffer process (and no value for *related-output-process*)")
      (kill-process process-to-kill) ) ) )
