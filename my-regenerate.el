(defun regenerate-this-file ()
  "Self-generate this file from regenerate.rb" 
  (interactive)
  (compile-this-file-using-ruby-script "-S" "regenerate") )

(defun dev-regenerate ()
  "Self-generate this file or directory from dev version of regenerate.rb" 
  (interactive)
  (compile-this-file-using-ruby-script (concat "-I" *regenerate-dir* "/lib") 
                                       (concat *regenerate-dir* "/bin/regenerate") ) )

(defun set-total-window-height (window height)
  (let ( (delta (- height (window-total-height window))) )
    (window-resize window delta) ) )

(defun compile-this-file-using-ruby-script (&rest ruby-script-args)
  "Compile this file or directory from RUBY-SCRIPT"
  (save-this-buffer-and-others)
  (let ( (filename (get-this-file-or-directory-name))
	 (file-buffer (current-buffer))
	 (ruby-executable (project-file :ruby-executable))
	 (ruby-args (project-value :ruby-args))
	 (base-directory (project-base-directory-value))
         (ruby-buffer (get-buffer-create "*ruby*")) )
    (message "Regenerating %s with ruby command %s ..." filename ruby-script-args)
    (display-buffer ruby-buffer)
    (save-selected-window
      (set-buffer ruby-buffer)
      (clear-buffer)
      (message "ruby-args = %s, ruby-script-args = %s, filename = %s" ruby-args ruby-script-args filename)
      (apply #'call-process 
             `(,ruby-executable nil (t t) t ,@ruby-args 
                                ,@ruby-script-args ,filename))
      (message "Finished regenerating %s" filename)
      (set-window-point (get-buffer-window ruby-buffer) (point-max))
      (set-total-window-height (get-buffer-window ruby-buffer) 6) )
    (revert-if-saved) ) )

;;-----------------------------------------------------------------
(defun create-display-date()
  (format-time-string "%-e %B, %Y") )

(defun replace-in-buffer (from-string to-string)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward from-string nil t)
      (replace-match to-string to-string t) ) ) )

(defun spacify-char (ch)
  "A helper function when spacifying camel case - inserts space before a capital letter"
  (if (and (>= ch 65) (<= ch 90))
      (concat " " (char-to-string ch))
    (char-to-string ch)) )

(defun spacify-camel-case(string)
  "Given a string like 'HelloThereWorld', return 'Hello There World'"
  (let ( (first-char (substring string 0 1))
         (rest (substring string 1) ) )
    (concat first-char
            (apply 'concat (mapcar 'spacify-char rest)) ) ) )

(defun create-title-from-file-name (file-name)
  "Create an initial title for a camel-cased file name"
  (spacify-camel-case
   (file-name-sans-extension (file-name-nondirectory file-name)) ) )

(defun new-web-page()
  "Start a new empty web page from _template.html in the same directory, filling in {DATE} and {TITLE} with initial values"
  (interactive)
  (insert-file-contents "_template.html")
  (replace-in-buffer "{DATE}" (create-display-date))
  (replace-in-buffer "{TITLE}" 
                     (create-title-from-file-name (buffer-file-name)) )
  (save-buffer) )
  
