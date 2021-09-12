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

(defun search-this-directory()
  (interactive)
  (run-project-command 'other-window-show-top 'current-dir "search" 'search-string-for-general-search) )

(defun project-search-for-identifier()
  (run-project-command 'other-window-show-top 'base-dir "search" 'identifier-for-search) )

(defun project-search-census()
  (interactive)
  (run-project-command 'other-window-show-top 'base-dir "search" 'search-census) )

(defun get-identifier-for-search()
  (or (project-identifier-at-point)
      (read-from-minibuffer "Search for: ") ) )

(defun get-python-regex-for-identifier (identifier)  ;; TODO actually use this somehow
  (format "\\b%s\\b" identifier) )

(defun get-search-python-script-path()
  (or (project-file :search-python-script-path) *search-python-script-path*) )

(defun get-base-search-args()
  (let ( (project-type (first-element-if-list (project-value :project-type 'default))) )
    (list (get-search-python-script-path) "." "--project-type" (symbol-name project-type) ) ) )

(defun get-identifier-for-search-args()
  (let ( (identifier-for-search (get-identifier-for-search)) )
    (append (get-base-search-args) (list "--value" identifier-for-search)) ) )  ;; TODO - regexes for identifiers

(defun get-search-census-args()
  (append (get-base-search-args) (list "--census")) )

(defun get-search-string-for-general-search-args()
  (let ( (search-string (read-from-minibuffer "Search for: ")) )
    (list (get-search-python-script-path) "." "--project-type" "general" "--include-unexpected" "--value" search-string) ) )

(def-run-project-fun 'command-args-getter 'search-census 'get-search-census-args)
(def-run-project-fun 'command-args-getter 'identifier-for-search 'get-identifier-for-search-args)
(def-run-project-fun 'command-args-getter 'search-string-for-general-search 'get-search-string-for-general-search-args)

