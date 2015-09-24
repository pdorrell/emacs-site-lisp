;; Copyright (C) 2001 Philip Dorrell

(defun make-show-search-command  (dirs extensions search-string)
  (list "4" "search" (separated-values dirs ";")
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
  '(".el" ".java" ".html" ".txt" ".xml" ".ftl" ".tex" ".py" ".sty" ".dtx" ".c" ".h" ".cpp" ".def" ".lisp" ".cls" ".ml" ".mli" ".rb" ".rhtml" ".rxml" ".js" ".hx" ".css" ".svg" ".drl" ".erb" ".md" ".haml" ".yml" ".treetop" ".scss" ".coffee" ".bracketup" ".corrml")
  "List of extensions for files to search")

(defun search-this-dir (string)
  (interactive "sSearch for: ")
  (show-search-buffer (list (file-truename default-directory)) *default-search-extensions* string) )

(global-set-key [?\C-w] 'search-this-dir)


(defvar *project-search-dirs* nil)
(defvar *project-search-extensions* nil)

(defun project-identifier-at-point ()
  (if saved-mouse-selection
      (get-saved-mouse-selection)
    (word-at word-alpha-table (point)) ) )

(defun project-search-for-identifier-at-point ()
  (interactive)
  (save-this-buffer-and-others)
  (let ( (identifier (project-identifier-at-point)) )
    (if identifier
	(project-search-for-identifier identifier)
      (call-interactively 'project-search-for-identifier) ) ) )

(defun project-search-for-identifier (identifier)
  (interactive "sSearch for: ")
  (show-search-buffer (cons (project-base-directory-value) (project-value :extra-search-directories))
		      (project-value :search-extensions) identifier) )

(global-set-key [M-f12] 'project-search-for-identifier-at-point)
