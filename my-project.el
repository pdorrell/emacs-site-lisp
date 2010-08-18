(set-default 'current-project nil)

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
  (let ( (filename (buffer-file-name)) )
    (if filename
	(file-name-directory filename)
      default-directory) ) )

(defvar *project-base-dir* nil 
  "A global variable which stores the current project base directly, while loading an existing project file. ")

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

(defun load-this-project (key-value-pairs)
  "This function is called from within a _project.el file with key/value pairs as a list of cons pairs. 
   It assumes that *project-base-dir* is defined.
   Can be invoked interatively from within the _project.el file to reload new project values."
  (if (not load-file-name)
      (setq *project-base-dir* (get-directory-for-project)) )
  (let ( (project (create-project key-value-pairs (gethash *project-base-dir* *projects*))) )
    (puthash :base-directory *project-base-dir* project)
    (puthash *project-base-dir* project *projects*) ) )

(defun find-current-project ()
  "Find the project object for the current buffer"
  (let* ( (project-dir-and-file (get-current-project-dir-and-file))
	  (project-dir (car project-dir-and-file))
	  (project-file (cdr project-dir-and-file)) )
    (if project-dir
	(let ( (project (gethash project-dir *projects*)) )
	  (if (not project)
	      (progn
		(setq *project-base-dir* project-dir)
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

(defun project-file (key)
  "Get the expanded name of a file from project value for KEY, expanded against project base directory (if it's relative)"
  (let ( (file-name (project-value key) ) )
    (if file-name
	(let ( (base-directory (project-base-directory)) )
	  (if base-directory
	      (expand-file-name file-name base-directory) 
	    nil) )
      nil) ) )

(defun project-directory (key)
  "Get the expanded name of a directory from project value for KEY, expanded against project base directory (if it's relative)"
  (let ( (directory-file (project-file key)) )
    (if directory-file
	(directory-file-name directory-file)
      nil) ) )

(defun project-base-directory()
  "Get base directory for the current project"
  (project-value :base-directory) )

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
	(eval run-project-command)
      (message "No run-project-command defined in this buffer") ) ) )

(global-set-key [?\M-p] 'visit-project-file)

(global-set-key [C-M-f9] 'run-this-file)
(global-set-key [S-M-f9] 'run-project)
