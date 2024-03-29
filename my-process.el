;; Copyright (C) 2008 Philip Dorrell

(defun set-process-killable-without-query(process)
  "Set flag so that PROCESS can be killed without a confirmation query"
  (set-process-query-on-exit-flag process nil) )

(defun start-process-if-not-going (name startup-command &optional move-to-top)
  "Start process with NAME in the current buffer if it doesn't already exist, using STARTUP-COMMAND.
   If MOVE-TO-TOP, then move to top (actually 1st unindented line) when that
   output appears."
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
    (set-process-killable-without-query process)
    (if move-to-top (set-process-filter process (make-goto-first-unindented-line-process-filter)))
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
  (make-regex '(seq "^" some-non-whitespace) "^[^ \t]+")
  "A regex matching start of a line that begins with a non-whitespace character")

(defun goto-first-unindented-line ()
  "Go to first line that doesn't start with white space (the assumption is that non-actionable 
   header lines start with whitespace and you want to sit on the first actionable output line)"
  (goto-char (point-min))
  (if (search-forward-regexp line-starting-with-non-whitespace-regex nil t)
      (progn (beginning-of-line) t)
    nil) )

(defun do-not-move-point-process-filter (process string)
  "A process filter that inserts a STRING at end of PROCESS buffer, without moving current point"
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert string) ) )

(defun make-goto-first-unindented-line-process-filter ()
  "Create process filter that inserts a string at the end of the process buffer, and, 
   the first time an unindented line appears in the output, goes to that line (otherwise
   dont' move current point)."
  (lexical-let ( (unindented-line-not-yet-found t) )
    (lambda (process string)
      (save-excursion
        (set-buffer (process-buffer process))
        (let ( first-unindented-line-pos )
          (save-excursion
            (goto-char (point-max))
            (insert string)
            (if unindented-line-not-yet-found
                (if (goto-first-unindented-line)
                    (setq first-unindented-line-pos (point)) ) ) )
          (when (and unindented-line-not-yet-found first-unindented-line-pos)
            (goto-char first-unindented-line-pos)
          (setq unindented-line-has-been-found t) ) ) ) ) ) )

(defun write-end-of-buffer-sentinel (process event)
  "A sentinel for handling process signals, which writes EVENT at the end of the PROCESS buffer
   (if the buffer exists and is 'live')."
  (let ( (buffer (process-buffer process)) )
    (if (and buffer (buffer-live-p buffer))
        (save-excursion
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert (concat "\nProcess " 
                            (process-name process) " " event)) ) ) ) ) )

(defun stop-then-start-process (name process-variable process-buffer-name
				     executable args &optional move-to-top)
  "If PROCESS-VARIABLE has a value, switch to buffer with name PROCESS-BUFFER-NAME for that process.
   If the process exists and is running, delete the process.
   Then, clear the process buffer and start a new process named NAME with EXECUTABLE and ARGS.
   If MOVE-TO-TOP, then move to 1st unindented line of output when it appears.
   Set the new process to be the value of PROCESS-VARIABLE."
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
                                      :filter (if move-to-top (make-goto-first-unindented-line-process-filter))
                                      :sentinel #'write-end-of-buffer-sentinel) ) )
      (set process-variable new-process)
      (message "%s STARTED" name) ) ) )

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
