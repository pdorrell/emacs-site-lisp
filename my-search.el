;; Copyright (C) 2001 Philip Dorrell

(defvar *search-python-script-path* (expand-file-name "bin" emacs-customisation-dir))

(defun project-identifier-at-point ()
  (if saved-mouse-selection
      (get-saved-mouse-selection)
    (word-at word-alpha-table (point)) ) )

(defun project-search-for-identifier-at-point ()
  (interactive)
  (save-this-buffer-and-others)
  (project-search-for-identifier) )

(defun project-search-for-definition-at-point ()
  (interactive)
  (save-this-buffer-and-others)
  (project-search-for-definition) )

(defun project-search-for-identifier-part-at-point ()
  (interactive)
  (save-this-buffer-and-others)
  (project-search-for-identifier-part) )

(defun search-this-directory()
  (interactive)
  (run-project-command 'other-window-show-top 'current-dir "search" 'search-string-for-general-search) )

(defun project-search-for-identifier()
  (run-project-command 'other-window-show-top 'base-dir "search" 'search-for-identifier) )

(defun project-search-for-identifier-part()
  (run-project-command 'other-window-show-top 'base-dir "search" 'search-for-identifier-part) )

(defun project-search-for-definition()
  (run-project-command 'other-window-show-top 'base-dir "search" 'search-for-definition) )

(defun project-search-census()
  (interactive)
  (run-project-command 'other-window-show-top 'base-dir "search" 'search-census) )

(defun get-identifier-for-search()
  (or (project-identifier-at-point)
      (read-from-minibuffer "Search for: ") ) )

(defun get-identifier-for-definition-search()
  (or (project-identifier-at-point)
      (read-from-minibuffer "Search for definition of: ") ) )

(defun get-python-regex-for-identifier (identifier)  ;; TODO actually use this somehow
  (format "\\b%s\\b" identifier) )

(defun get-search-python-script-path()
  (or (project-file :search-python-script-path) *search-python-script-path*) )

(defun get-base-search-args()
  (let ( (project-type (first-element-if-list (project-value :project-type 'default))) )
    (list (get-search-python-script-path) "." "--project-type" (symbol-name project-type) ) ) )

(def-run-project-fun 'command-args-getter 'search-for-identifier
  (defun get-identifier-for-search-args()
    (let* ( (identifier-at-point (project-identifier-at-point))
            (language-regexes (gethash programming-language *language-search-regexes*))
            (search-pattern-args 
             (if (and identifier-at-point language-regexes)
                 (list (concat "--before-regex=" (gethash :before-identifier language-regexes))
                       (concat "--value=" identifier-at-point)
                       (concat "--value-description=" identifier-at-point)
                       (concat "--after-regex=" (gethash :after-identifier language-regexes)) )
               (let ( (search-string (or identifier-at-point
                                         (read-from-minibuffer "Search for: ") )) )
                 (list (concat "--value=" search-string)) ) ) ) )
      (append (get-base-search-args) search-pattern-args) ) ) )

(def-run-project-fun 'command-args-getter 'search-for-identifier-part
  (defun get-identifier-part-for-search-args()
    (let* ( (search-string 
             (or (project-identifier-at-point)
                 (read-from-minibuffer "Search for: ") ) )
            (search-pattern-args (list (concat "--value=" search-string))) )
      (append (get-base-search-args) search-pattern-args) ) ) )

(def-run-project-fun 'command-args-getter 'search-for-definition
  (defun get-identifier-for-definition-search-args()
    (let* ( (identifier-for-search (get-identifier-for-definition-search))
            (language-regexes (gethash programming-language *language-search-regexes*)) )
      (if (not language-regexes)
          (error "No language search regexes defined for language %S" programming-language) )
      (let ( (search-pattern-args 
              (list "--before-regex" (gethash :before-definition language-regexes)
                    "--value" identifier-for-search
                    "--value-description" (concat "(defn) " identifier-for-search)
                    "--after-regex" (gethash :after-definition language-regexes) ) ) )
        (append (get-base-search-args) search-pattern-args) ) ) ) )

(def-run-project-fun 'command-args-getter 'search-census
  (defun get-search-census-args()
    (append (get-base-search-args) (list "--census")) ) )

(def-run-project-fun 'command-args-getter 'search-string-for-general-search
  (defun get-search-string-for-general-search-args()
    (let ( (search-string (read-from-minibuffer "Search for: ")) )
      (list (get-search-python-script-path) "." "--project-type" "general" 
            "--include-unexpected" "--value" search-string) ) ) )

(defun project-create-project-search-spec-file (project-type)
  (let* ( (project-base-directory (get-current-project-base-directory))
          (project-type-search-spec-file-name (concat project-type ".search-spec.json"))
          (project-search-spec-directory (expand-file-name "_project/search" project-base-directory))
          (project-search-spec-file-name (expand-file-name project-type-search-spec-file-name 
                                                           project-search-spec-directory))
          (search-spec-template-file-name (expand-file-name "search-specs/empty-template.search-spec.json"
                                                            emacs-customisation-dir) ) )
    (if (file-exists-p project-search-spec-file-name)
        (error "File %S already exists" project-search-spec-file-name) )
    (make-directory project-search-spec-directory t)
    (copy-file search-spec-template-file-name project-search-spec-file-name)
    (message "Project search spec file %s created" project-search-spec-file-name) ) )
          
(defvar *language-search-regexes* nil "Table of tables of regexes used for search in each programming language")
(setq *language-search-regexes* (make-hash-table :test 'eq))

(cl-defun set-language-search-regexes (language &key before-identifier after-identifier before-definition after-definition)
  (declare (indent defun))
  (let ( (regex-hash-table (make-hash-table :test 'eq)) )
    (puthash :before-identifier before-identifier regex-hash-table)
    (puthash :after-identifier after-identifier regex-hash-table)
    (puthash :before-definition before-definition regex-hash-table)
    (puthash :after-definition after-definition regex-hash-table)
    (puthash language regex-hash-table *language-search-regexes*) ) )

