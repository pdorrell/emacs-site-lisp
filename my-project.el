;; A project object is a hashtable consisting of key/value pairs, defined for a project.
;; Each project is defined by a _project.el file in the base directory of the project,
;; which should include a call to load-this-project.

;; Projects is a table of all the project objects, indexed by project base directory
(defvar *projects* (make-hash-table :test 'equal))

(defvar *project-by-base-directory* (make-hash-table :test 'equal))

(defvar *project-definition-file-by-base-directory* (make-hash-table :test 'equal))

(defvar *project-base-directory-by-directory* (make-hash-table :test 'equal))

(defvar current-project nil 
  "The current project object for the project which owns the file being edited (or default directory if there is no file) in the current buffer")
(set-default 'current-project nil)
(make-variable-buffer-local 'current-project)

(defvar current-project-base-directory nil
  "Directory of current project")
(set-default 'current-project-base-directory nil)
(make-variable-buffer-local 'current-project-base-directory)

(defvar current-project-definition-file nil
  "Current project definition file")
(set-default 'current-project-definition-file nil)
(make-variable-buffer-local 'current-project-definition-file)

(defun get-current-directory()
  "Find current directory to be used as starting point to search for project file.
   If visiting a file, use directory of buffer file name (usually default-directory, put sometimes isn't),
   otherwise just use default-directory"
  (let* ( (filename (buffer-file-name))
	  (directory-name (if filename
			      (file-name-directory filename)
			    default-directory)) )
    (expand-file-name directory-name) ) )

(defun get-current-project-base-directory()
  (when (not current-project-base-directory)
    (let ( (current-directory (get-current-directory)) )
      (setq current-project-base-directory 
            (gethash current-directory *project-base-directory-by-directory*))
      (if (not current-project-base-directory)
          (load-current-project-base-directory current-directory) ) ) )
  current-project-base-directory)

(defun load-current-project-base-directory (current-directory)
  (cons-bind project-directory project-definition-file-name
    (find-project-directory-and-definition-file current-directory)
    (puthash current-directory project-directory *project-base-directory-by-directory*)
    (puthash project-directory project-definition-file-name *project-definition-file-by-base-directory*)
    (setq current-project-base-directory project-directory)
    (setq current-project-definition-file project-definition-file-name) ) )

(defun get-current-project-definition-file()
  (when (not current-project-definition-file)
    (get-current-project-base-directory)
    (setq current-project-definition-file
          (gethash current-project-base-directory *project-definition-file-by-base-directory*) ) )
  current-project-definition-file)
  
(defun get-current-project()
  (when (not current-project)
    (get-current-project-base-directory)
    (if current-project-base-directory
        (setq current-project
              (get-project-for-base-directory current-project-base-directory) ) ) )
  current-project)

(defun get-project-for-base-directory (base-directory)
  (let ( (project (gethash base-directory *project-by-base-directory*)) )
    (if (not project)
        (let ( (project-definition-file (gethash base-directory *project-definition-file-by-base-directory*)) )
          (when project-definition-file
              (load-project-from-definition-file base-directory project-definition-file)
              (setq project (gethash base-directory *project-by-base-directory*)) ) ) )
    project) )

(defun load-project-from-definition-file (base-directory project-definition-file)
  (let ( (*project-base-directory* base-directory) )
    (load project-definition-file) ) )

(defvar *project-definition-file-names* '("_project.el" "__project.el")
  "Possible names for project definition files - first one is default value for new project files.")

(defvar *project-marker-subdirectories* '(".git")
  "Sub-directories, if they exist, which mark the base directory of a project")

(defun-getting-value find-project-directory-and-definition-file(directory)
  "Return the project directory and project definition file for a directory as a cons pair - the directory is nil if no project marker
is found at all, the definition file is nil if the project is defined by a sub-directory marker"
  (let ( (dir-to-check directory) )
    (while (not (equal dir-to-check "/"))
      (dolist (project-definition-file-name *project-definition-file-names*)
        (let ( (definition-file (expand-file-name project-definition-file-name dir-to-check)) )
             (if (file-exists-p definition-file)
                 (return-value (cons dir-to-check definition-file)) ) ) )
      (dolist (project-marker-subdirectory *project-marker-subdirectories*)
        (let ( (sub-directory (expand-file-name project-marker-subdirectory dir-to-check)) )
          (if (file-exists-p sub-directory)
              (return-value (cons dir-to-check nil)) ) ) )
      (setq dir-to-check (file-name-directory (directory-file-name dir-to-check))) ) )
  (return-value (cons nil nil)) )

(run-test
 (find-project-directory-and-definition-file (test-file "test-project/src/subdir/"))
 (cons (test-file "test-project/") (test-file "test-project/_project.el")) )

(defun create-project (key-value-pairs &optional existing-project)
  "Create (or re-create) a project object from given key/value pairs, optionally re-using an existing project if supplied"
  (let ( (project (if existing-project 
		      (clrhash existing-project) 
		    (make-hash-table :test 'equal))) )
    (dolist (key-value-pair key-value-pairs)
      (puthash (first key-value-pair) (second key-value-pair) project) )
    (let ( (project-type-value (gethash :project-type project)) )
      (if project-type-value
          (let ( (project-types (listify-if-not-list project-type-value)) )
            (dolist (project-type (reverse project-types)) ;; in reverse, so that last type takes priority
              (let ( (project-defaults (get-project-type-default-values project-type) ) )
                (if project-defaults
                    (dolist (key-value-pair project-defaults)
                      (cl-destructuring-bind (key value) key-value-pair
                        (if (not (gethash key project))  ;; apply value if one is not yet there
                            (puthash key value project) ) ) )
                  (error "No default project of type %S" project-type) ) ) ) ) )
    project) ) )

(defvar *default-project* (create-project `((:python-executable ,*python-executable*)))
  "The default project, which will contain default values for project values.")

(defvar *project-base-directory* nil 
  "A variable which stores the current project base directly, while loading an existing project file. ")

(defun load-this-project (key-value-pairs)
  "This function is called from within a _project.el file with key/value pairs as a list of cons pairs. 
   It assumes that *project-base-directory* is defined.
   Except, it can be invoked interatively from within the _project.el file to reload new project values,
   in which case *project-base-directory* is set to be the current directory."
  (let* ( (*project-base-directory* (if load-file-name *project-base-directory* 
                                      (file-name-directory (buffer-file-name)) ) )
          (project (create-project key-value-pairs (gethash *project-base-directory* *projects*))) )
    (puthash *project-base-directory* project *project-by-base-directory*)
    (puthash *project-base-directory* project *projects*) ) )

(defun project-value (key &optional default)
  "Get the value for KEY in the current project, or from the default project if there is no current project."
  (let* ( (project (or (get-current-project) *default-project*))
	  (value (or (gethash key project) default)) )
    value) )

(defun visit-project-file ()
  "Visit the current project file (or offer to create one if it can't be found)"
  (interactive)
  (let ( (project-definition-file (get-current-project-definition-file)) )
    (if project-definition-file
	(find-file project-definition-file)
      (progn
        (message "No project file exists at %s" current-project-base-directory)
        (maybe-create-new-project-file) ) ) ) )

(defun visit-project-file-menu ()
  "Visit file-menu file for project"
  (interactive)
  (let* ( (project-base-directory (get-current-project-base-directory))
          (file-menu-file (expand-file-name "_" project-base-directory)) )
    (if (file-exists-p file-menu-file)
        (progn
          (find-file file-menu-file)
          (message file-menu-file) )
      (progn
        (find-file project-base-directory)
        (message "File menu %s does not exist" file-menu-file) ) ) ) )

;; TODO - copy from a template file
(defun maybe-create-new-project-file()
  "Offer to create a project file in the current directory (which the user can edit before accepting)"
  (interactive)
  (let* ( (existing-project-directory (get-current-project-base-directory))
          (project-dir (read-directory-name "No project file found, create new one in directory: " 
                                            existing-project-directory) ) )
    (find-file (expand-file-name (car *project-definition-file-names*) project-dir))
    (insert ";; Project values\n\n(load-this-project\n `( (:project-type default)\n     (:key \"value\") ) )\n") ) )

(make-variable-buffer-local 'run-file-function) ;; TODO - not project specific ?


;;====================================================================================================

(defun project-file (key &optional default)
  "Get the expanded name of a file from project value for KEY, expanded against project base directory (if it's relative)"
  (let ( (file-name (project-value key default) ) )
    (if file-name
	(let ( (base-directory (get-current-project-base-directory)) )
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

(defun project-required-value (key)
  (let ( (value (project-value key)) )
    (if (not value)
	(error "No value found for project key %s" key)
      value) ) )

(defun open-project-file-menu()
  (interactive)
  (find-file (expand-file-name "_" (get-current-project-base-directory))) )

(defun build-project() ;; 'make'
  (interactive)
  (apply (project-required-value :build-function) nil) )

(defun build-project-with-target (target) ;; 'make' with target
  (interactive "starget: ")
  (apply (project-value :build-function) (list target)) )

(defun project-buffers-menu()
  "Display buffers with a default-directory contained within project-base-dir"
  (interactive)
  (let ( (project-base-directory (get-current-project-base-directory)) )
    (ibuffer-list-buffers)
    (switch-to-buffer "*Ibuffer*")
    (ibuffer-filter-disable)
    (ibuffer-filter-by-predicate
     `(equal (get-current-project-base-directory) ,project-base-directory) ) ) )

