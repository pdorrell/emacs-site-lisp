;; Copyright (C) 2000,2001 Philip Dorrell

;;========================================================================

(defvar rake-command "rake")
(defvar rake-file "rakefile")
(defvar *current-output-buffer* nil "Last buffer which for the output of running or compiling something")

(message (file-exists-p (concat default-directory "rakefile")))

(list default-directory)

(file-name-directory (substring default-directory 0 -1))

(defun find-parent-directory-with-file (dir filename)
  (let ( (parent dir) (found nil) (finished nil))
    (while (and (not found) (not finished))
      (if (file-exists-p (concat parent filename))
	  (setq found t)
	(let ( (newparent (file-name-directory (substring parent 0 -1))) )
	  (setq finished (equal newparent parent))
	  (setq parent newparent) ) ) )
    (and found parent)) )

(defun compile-with-command (command &optional command-file)
  (let ( (project-base-dir (project-base-directory)) )
    (save-this-buffer-and-others)
    (if command-file
	(let ( (command-file-dir (find-parent-directory-with-file default-directory command-file)) )
	  (if command-file-dir
	      (find-file (concat command-file-dir command-file)) ) ) )
    (compile command)
    (other-window 1)
    (switch-to-buffer "*compilation*")
    (if project-base-dir
	(progn
	  (message "In compile-with-command, cd compilation buffer to %s" project-base-dir)
	  (cd project-base-dir) ) )
    (setq *current-output-buffer* "*compilation*")
    (end-of-buffer) ) )
  
(defun compile-rake (&optional target)
  "Compile using rake"
  (compile-with-command (if target (concat rake-command " " target) rake-command)) )

(defun compile-ant ()
  "Compile using ant"
  (interactive)
  (compile-with-command "ant -emacs -find build.xml") )

(defvar ant-before-target "" "extra stuff to put before ant target")
(defvar ant-after-target "" "extra stuff to put after ant target")

(defun compile-ant-target (target)
  "Compile using ant"
  (interactive "starget: ")
  (compile-with-command (concat "ant -emacs -find build.xml " target)) )

(defun previous-error ()
  "Go to previous error"
  (interactive)
  (next-error -1) )

(defun visit-output-buffer()
  (interactive)
  (if *current-output-buffer*
      (show-buffer nil *current-output-buffer*)
    (message "No current output buffer to visit") ) )

(defun git-gui-this-directory()
  "Start git gui in this directory"
  (interactive)
  (set-process-query-on-exit-flag (start-process "gitgui" nil *git-executable* "gui") nil) )

(defun gitk-this-directory()
  "Start gitk in this directory"
  (interactive)
  (set-process-query-on-exit-flag (start-process "gitk" nil *gitk-executable* "--all") nil) )

(defun run-alternate-command()
  "Compile this file"
  (interactive)
  (eval (project-required-value :alternate-run-command)) )

(defun insert-tranformed-word (word-table transformer description)
  "Replace preceding word in buffer (according to WORD-TABLE) by applying TRANSFORMER to it.
  If no word is found to replace, warn that the DESCRIPTION is not there."
  (let* ( (word (word-before word-table (point))) )
    (if word
	(progn
	  (delete-backward-char (length word))
	  (insert (funcall transformer word)) )
      (message "No %s given" descriptipon) ) ) )

(define-minor-mode stop-on-1st-test-failure-minor-mode
  "When running unit tests, stop on first failure"
  nil " Stop1stFail")
