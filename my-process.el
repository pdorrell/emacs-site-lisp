;; Copyright (C) 2008 Philip Dorrell

(defvar emacs-util-classpath "." "Class path for list methods utility")

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
    (if move-to-top (set-process-filter process 'goto-first-line-process-filter))
    process) )
  

(defun run-process-with-line (name startup-command input-line &optional move-to-top)
  (let ( (process (start-process-if-not-going name startup-command move-to-top)) )
    (clear-buffer)
    (process-send-string process (concat input-line "\n"))
    process) )

(defun run-process-with-lines (name startup-command input-lines &optional move-to-top clear-buffer)
  (let ( (process (start-process-if-not-going name startup-command move-to-top)) )
    (if clear-buffer (clear-buffer))
    (dolist (input-line input-lines)
      (process-send-string process (concat input-line "\n")) )
    process) )

(defun goto-first-line ()
  (goto-char (point-min))
  (search-forward-regexp line-completion-first-line-regexp nil t)
  (beginning-of-line) )

(defun goto-first-line-process-filter (process string)
  (let ( (old-buffer (current-buffer))
	 (buffer (process-buffer process)) )
    (set-buffer buffer)
    (goto-char (point-max))
    (insert string)
    (goto-first-line)
    (set-buffer old-buffer) ) )

(defun stop-start-process (name process-variable process-buffer-name
				executable args)
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
	  (message "%s STARTED" name) ) ) )) )

(defun stop-then-start-process (name process-variable process-buffer-name
				     executable args)
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
    (insert (format "%s %s\n" executable args))
    (message "Starting new process %s %s" executable args)
    (let ( (new-process (apply #'start-process name process-buffer-name executable args)) )
      (set process-variable new-process)
      (set-process-query-on-exit-flag new-process nil)
      (message "%s STARTED" name) ) ) )

(defun start-process-showing-console (name process-buffer-name
				     executable &rest args)
  "Start a server process, where any stopping of existing processes is handled by the script"
  (save-this-buffer-and-others)
  (if (not (equal process-buffer-name (buffer-name (current-buffer))))
      (progn
	(switch-to-buffer-other-window process-buffer-name) ) )
  (clear-buffer)
  (insert (format "%s %s\n" executable args))
  (message "Starting process %s %s" executable args)
  (let ( (new-process (apply #'start-process name process-buffer-name executable args)) )
    (set-process-query-on-exit-flag new-process nil)
    (message "%s STARTED" name) ) )

