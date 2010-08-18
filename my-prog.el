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

(defun compile-with-command (command command-file)
  (let ( (command-file-dir (find-parent-directory-with-file default-directory command-file)) )
    (if command-file-dir
	(setq default-directory command-file-dir) ) )
  (save-this-buffer-and-others)
  (message "about to compile #%s#" command)
  (compile command)
  (other-window 1)
  (switch-to-buffer "*compilation*")
  (setq *current-output-buffer* "*compilation*")
  (end-of-buffer) )
  
(defun compile-rake (&optional target)
  "Compile using rake"
  (compile-with-command (if target (concat rake-command " " target) rake-command) rake-file) )

(defun compile-ant ()
  "Compile using ant"
  (interactive)
  (compile-with-command "ant -emacs -find build.xml" "build.xml") )

(defvar ant-before-target "" "extra stuff to put before ant target")
(defvar ant-after-target "" "extra stuff to put after ant target")

(defun compile-ant-target (target)
  "Compile using ant"
  (interactive "starget: ")
  (compile-with-command (concat "ant -emacs -find build.xml " target) "build.xml") )

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
  (start-process "gitgui" nil *git-executable* "gui") )
  

;;========================================================================
(global-set-key [?\M-k] 'visit-output-buffer)
(global-set-key [?\M-N] 'compile-ant)
(global-set-key [?\C-\M-N] 'compile-ant-target)
(global-set-key [f12] 'next-error)
(global-set-key [pause] 'next-error)
(global-set-key [S-pause] 'previous-error) 
(global-set-key [C-M-f10] 'comment-region)
(global-set-key [?\C-\M-G] 'git-gui-this-directory)
