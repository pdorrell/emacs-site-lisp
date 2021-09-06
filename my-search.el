;; Copyright (C) 2001 Philip Dorrell

(setq *search-python-script-path* (expand-file-name "bin" emacs-customisation-dir))

(defun make-search-dir-with-excludes (dir-spec)
  (if (stringp dir-spec)
      dir-spec
    (separated-values dir-spec "::") ) )

(defun make-show-search-command  (dirs extensions search-string)
  (list "4" "search" (separated-values (mapcar #'make-search-dir-with-excludes dirs) ";")
	 (separated-values extensions ";")
	 search-string) )

(defun show-search-buffer (dirs extensions search-string)
  (setq search-previous-buffer (current-buffer))
  (switch-to-buffer "*search*")
  (local-set-key [?\M-K] 'bury-buffer)
  (let ( (startup-command (list *java-executable* "-cp" emacs-util-classpath "com1729.emacs.search.StringSearch"))
	 (input-lines (make-show-search-command dirs extensions search-string)) )
    (run-process-with-lines "search" startup-command input-lines nil t) ) )

(defvar *default-search-extensions*
  '(".el" ".java" ".html" ".txt" ".csv" ".xml" ".ftl" ".tex" ".py" ".sty" ".dtx" ".c" ".h" ".cpp" ".def" ".lisp" ".cls" ".ml" ".mli" ".rb" ".rhtml" ".rxml" ".js" ".hx" ".css" ".svg" ".drl" ".erb" ".md" ".haml" ".yml" ".scss" ".bracketup" ".corrml" ".idr" ".ipkg" ".brackdom" ".hs" ".json")
  "List of extensions for files to search")

(defun search-this-dir (string)
  (interactive "sSearch for: ")
  (show-search-buffer (list (file-truename default-directory)) *default-search-extensions* string) )

(defvar *project-search-dirs* nil)
(defvar *project-search-extensions* nil)

(defun project-identifier-at-point ()
  (if saved-mouse-selection
      (get-saved-mouse-selection)
    (word-at word-alpha-table (point)) ) )

(defun project-search-for-identifier-at-point ()
  (interactive)
  (save-this-buffer-and-others)
  (if (project-value :new-search)
      (project-new-search-for-identifier)
    (project-search-for-identifier) ) )

;;(let ( (value (read-from-minibuffer "Value: ")) ) (message "You told me %S" value) ) 

(defun project-new-search-for-identifier()
  (run-project-command 'other-window-show-top 'base-dir "search" 'identifier-for-search) )

(defun project-new-search-census()
  (interactive)
  (run-project-command 'other-window-show-top 'base-dir "search" 'search-census) )

(defun get-identifier-for-search()
  (or (project-identifier-at-point)
      (read-from-minibuffer "Search for: ") ) )

(defun get-python-regex-for-identifier (identifier)
  (format "\\b%s\\b" identifier) )

(defun get-base-search-args()
  (let ( (project-type (project-value :project-type 'python))
         (search-python-script-path (or (project-file :search-python-script-path)
                                        *search-python-script-path*)) )
    (list search-python-script-path "." "--project-type" (symbol-name project-type) ) ) )

(defun get-identifier-for-search-args()
  (let ( (identifier-for-search (get-identifier-for-search)) )
    (append (get-base-search-args) (list "--value" identifier-for-search)) ) )  ;; TODO - regexes for identifiers

(defun get-search-census-args()
  (append (get-base-search-args) (list "--census")) )

(def-run-project-fun 'command-args-getter 'search-census 'get-search-census-args)
(def-run-project-fun 'command-args-getter 'identifier-for-search 'get-identifier-for-search-args)

(expand-file-name "./test" default-directory)

(defun project-search-for-identifier()
  (let* ( (identifier (get-identifier-for-search))
          (main-search-dir (project-base-directory-value))
          (exclude-subdirs (project-value :search-exclude-subdirs)) )
    (if exclude-subdirs
        (setq main-search-dir (cons main-search-dir exclude-subdirs)) )
    (message "exclude-subdirs = %s" exclude-subdirs)
    (show-search-buffer (cons main-search-dir (project-value :extra-search-directories))
                        (project-value :search-extensions) identifier) ) )

