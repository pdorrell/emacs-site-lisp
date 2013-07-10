;; A project object is a hashtable consisting of key/value pairs, defined for a project.
;; Each project is defined by a _project.el file in the base directory of the project,
;; which should include a call to load-this-project.

;; Projects is a table of all the project objects, indexed by project base directory
(defvar *projects* (make-hash-table :test 'equal))

(defvar current-project nil 
  "The current project object for the project which owns the file being edited (or default directory if there is no file) in the current buffer")
(set-default 'current-project nil)
(make-variable-buffer-local 'current-project)

(defun create-project (key-value-pairs &optional existing-project)
  "Create a project object from given key/value pairs, optionally re-using an existing project if supplied"
  (let ( (project (if existing-project 
		      (clrhash existing-project) 
		    (make-hash-table :test 'eq))) )
    (dolist (key-value-pair key-value-pairs)
      (puthash (first key-value-pair) (second key-value-pair) project) )
    project) )

(defvar *default-project* (create-project `((:python-executable ,*python-executable*)))
  "The default project, which will contain default values for project values.")

(defun get-directory-for-project()
  "Find directory to be used as starting point to search for project file (either directory of buffer file,
other wise the current directory for the buffer)."
  (let* ( (filename (buffer-file-name))
	  (directory-name (if filename
			      (file-name-directory filename)
			    default-directory)) )
    (expand-file-name directory-name) ) )

(defvar *project-base-directory* nil 
  "A variable which stores the current project base directly, while loading an existing project file. ")

(defun get-current-project-dir-and-file()
  "Return the project directory and project file as a cons pair, or nil if no project file can be found."
  (let ( (directory (get-directory-for-project)) 
	 (project-file-not-found nil)
	 project-lisp-file)
    (block nil
      (while (not project-file-not-found)
	(setq project-lisp-file (concat directory "_project.el"))
	(if (file-exists-p project-lisp-file)
	    (return (cons directory project-lisp-file)) )
	(let ( (parent-directory (file-name-directory (directory-file-name directory))) )
	  (if (equal parent-directory directory)
	      (setq project-file-not-found t)
	    (setq directory parent-directory) ) ) )
      nil) ) )

(defun project-base-directory()
  "Get base directory of current project, _without_ loading the project definition."
  (if *project-base-directory*
      *project-base-directory*
    (car (get-current-project-dir-and-file)) ) )

(defun project-base-directory-value()
  "Get base directory of current project, as a project value. 
(Don't call this _within_ a project definition, because it will then recursively attempt to load
       the project definnition.)"
  (project-value :base-directory) )

(defun load-this-project (key-value-pairs)
  "This function is called from within a _project.el file with key/value pairs as a list of cons pairs. 
   It assumes that *project-base-directory* is defined.
   Except, it can be invoked interatively from within the _project.el file to reload new project values,
   in which case *project-base-directory* is set to be the current directory."
  (let ( (*project-base-directory* (if load-file-name *project-base-directory* (get-directory-for-project))) )
    (let ( (project (create-project key-value-pairs (gethash *project-base-directory* *projects*))) )
      (puthash :base-directory *project-base-directory* project)
      (puthash *project-base-directory* project *projects*) ) ) )

(defun find-current-project ()
  "Find the project object for the current buffer"
  (let* ( (project-dir-and-file (get-current-project-dir-and-file))
	  (project-dir (car project-dir-and-file))
	  (project-file (cdr project-dir-and-file)) )
    (if project-dir
	(let ( (project (gethash project-dir *projects*)) )
	  (if (not project)
	      (let ( (*project-base-directory* project-dir) )
		(message "Loading project %s from %s ..." project-dir project-file)
		(load project-file)
		(setq project (gethash project-dir *projects*))
		(if (not project)
		    (error "Failed to load project %s from %s" project-dir project-file) ) ) )
	  project)
      *default-project*) ) )

(defun current-project ()
  "Get the project object for the current buffer, first looking in the buffer-local variable current-project"
  (if (not current-project)
      (setq current-project (find-current-project)) )
  current-project)

(defun project-value (key &optional default)
  "Get the value for KEY in the current project, or from the default project if there is no current project."
  (let* ( (project (current-project) )
	  (value (gethash key project)) )
    (if (not value)
	(if default
	    (setq value default)
	  (setq value (gethash key *default-project*)) ) )
    value) )

(defun project-required-value (key)
  (let ( (value (project-value key)) )
    (if (not value)
	(error "No value found for project key %s" key)
      value) ) )

(defun project-file (key &optional default)
  "Get the expanded name of a file from project value for KEY, expanded against project base directory (if it's relative)"
  (let ( (file-name (project-value key default) ) )
    (if file-name
	(let ( (base-directory (project-base-directory)) )
	  (if base-directory
	      (setq file-name (expand-file-name file-name base-directory)) ) )
      )
    file-name) )

(defun project-directory (key)
  "Get the expanded name of a directory from project value for KEY, expanded against project base directory (if it's relative)"
  (let ( (directory-file (project-file key)) )
    (if directory-file
	(directory-file-name directory-file)
      nil) ) )

(defun visit-project-file ()
  "Visit the current project file (or offer to create one if it can't be found)"
  (interactive)
  (let ( (project-dir-and-file (get-current-project-dir-and-file)) )
    (if project-dir-and-file
	(find-file (cdr project-dir-and-file))
      (maybe-create-new-project-file) ) ) )

(defun maybe-create-new-project-file()
  "Offer to create a project file in the current directory (which the user can edit before accepting)"
  (interactive)
  (let ( (project-dir (read-file-name "No project file found, create new one in directory: ")) )
    (find-file (concat project-dir "_project.el"))
    (insert ";; Project values\n\n(load-this-project\n `( (:key \"value\") ) )\n") ) )

(make-variable-buffer-local 'run-file-function)

(defun run-this-file()
  (interactive)
  (save-this-buffer-and-others)
  (if run-file-function
      (apply run-file-function (list (buffer-file-name)))
    (message "No run-file-function defined in this buffer") ) )

(defun run-project()
  (interactive)
  (save-this-buffer-and-others)
  (let ( (run-project-command (project-value :run-project-command)) )
    (if run-project-command
	(progn
	  (message "Running project command %s ..." run-project-command)
	  (eval run-project-command) )
      (message "No run-project-command defined in this buffer") ) ) )

(defun show-project-log-buffer()
  (interactive)
  (apply *show-project-log-buffer-function* nil) )

(defun open-project-file-menu()
  (interactive)
  (find-file (concat (project-value :base-directory) "_")) )

(defun build-project()
  (interactive)
  (apply (project-value :build-function) nil) )

(defun build-project-with-target (target)
  (interactive "starget: ")
  (apply (project-value :build-function) (list target)) )

(defun project-compile-with-command (&optional target)
  "Compile using a command"
  (let ( (compile-command (project-value :compile-command)) )
    (compile-with-command (if target (concat compile-command " " target) compile-command)) ) )

(defun run-database-for-project ()
  "Run database for this project"
  (interactive)
  (let ( (run-database-command (project-required-value :run-database-command)) )
    (eval run-database-command) ) )

(defvar *mongod-process* nil "Running Mongo server")

(defun mongod (port directory)
  "Run mongod (for development) on PORT and directory"
    (switch-to-buffer-other-window "*mongod*")
    (clear-buffer)
    (stop-then-start-process "mongod" '*mongod-process* "*mongod*"
			     "mongod" (list "-port" (number-to-string port) "-dbpath" directory) ) )
  

(global-set-key [?\M-M] 'build-project)
(global-set-key [?\C-\M-M] 'build-project-with-target)

(global-set-key [?\M-p] 'visit-project-file)

(global-set-key [C-M-f9] 'run-this-file)
(global-set-key [S-M-f9] 'run-project)
(global-set-key [C-S-M-f8] 'run-database-for-project)
(global-set-key [C-M-f7] 'open-project-file-menu)

(global-set-key [?\C-\M-o] 'show-project-log-buffer)
