;-----------------------------------------------------------------
;; Each Line Matcher is a list of a regex, and 2 group numbers, 
;; for the file name and the line number (matching what is output by some program
;; like a compiler or a search application)


(defconst python-line-matcher 
  (list 
   (make-regex 
    '(seq any-spaces "File" any-spaces "\"" (group "[^\"]*") "\"," any-spaces "line" any-spaces (group int))
    "[ ]*File[ ]*\"\\([^\"]*\\)\",[ ]*line[ ]*\\([0-9]+\\)")
   1 2)
  "Line matcher for output of Python stack traces")

(defun visit-file-at-line-number (file-name line-number-string)
  "Visit file with name FILE-NAME and go to line with number LINE-NUMBER-STRING.
   (A relative file path will be interpreted relative to the default directory of the buffer.)"
  (let ( (line-number (string-to-number line-number-string)) )
    ;; (message "visiting file \"%s\" line %s ..." file-name line-number)
    (find-file file-name)
    (goto-line line-number)
    ) )

(defconst prefixed-grep-n-line-matcher 
  (list
   (make-regex
    '(seq start any-whitespace (shy-group (one-of "from " "")) (group "[^:]+") ":" (group int) ":?")
    "^[ \t]*\\(?:from \\|\\)\\([^:]+\\):\\([0-9]+\\):?")
   1 2) 
  "Line matcher for output lines in style of 'grep -n' but with extra optional prefix")

(defconst grep-n-line-matcher 
  (list 
   (make-regex
    '(seq start (group "[^:]+") ":" (group int) ":?")
    "^\\([^:]+\\):\\([0-9]+\\):?")
	1 2)
  "Line matcher for output lines in style of 'grep -n'")

(defvar file-line-matchers
  '(
    (visit-file-at-line-number grep-n-line-matcher)
    (visit-file-at-line-number python-line-matcher)
    )
  "List of file line-matchers to use for current buffer, being a list
   of pairs of name of function to act on parsed location and name of variable holding 
   list of regex and group numbers to parse current buffer line for args to pass to the function."
  )

(make-variable-buffer-local 'file-line-matchers)

(defun visit-file-line ()
  "Visit line of file given in grep output"
  (interactive)
  (let ( (fun-and-args
	  (block nil
	    (save-excursion
	      (beginning-of-line)
	      (dolist (file-line-matcher file-line-matchers)
		(let* ( (matcher (symbol-value (second file-line-matcher)))
			(this-args (match-regexp matcher)) )
		  (if this-args
		      (return (cons (first file-line-matcher) this-args)) ) ) ) ) ) ) )
    (if fun-and-args
	(let ( (fun (car fun-and-args))
	       (args (cdr fun-and-args)) )
	  (apply fun args) )
      (message "Failed to match on any of %S" file-line-matchers) ) ) )
