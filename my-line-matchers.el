;-----------------------------------------------------------------
;; Each Line Matcher is a list of a regex, and 2 group numbers, 
;; for the file name and the line number (matching what is output by some program
;; like a compiler or a search application)


(setq python-line-matcher 
      (list "[ ]*File[ ]*\"\\([^\"]*\\)\",[ ]*line[ ]*\\([0-9]+\\)"
	    1 2) )

(defun visit-grep-n-line (file-name line-number-string)
  "Visit file with name FILE-NAME and go to line with number LINE-NUMBER-STRING.
   (A relative file path will be interpreted relative to the default directory of the buffer.)"
  (let ( (line-number (string-to-number line-number-string)) )
    ;; (message "visiting file \"%s\" line %s ..." file-name line-number)
    (find-file file-name)
    (goto-line line-number)
    ) )

;; [ \\t\\n]*\\(from \\)?
(defconst grep-n-matcher (list "[ \t]*\\(?:from \\|\\)\\([a-zA-Z]?[:]?[^:]+\\):\\([0-9]+\\):?"
			       1 2) 
  "Line matcher for output lines in style of 'grep -n'")

(setq unprefixed-grep-n-matcher (list "^\\([a-zA-Z]?[:]?[^: ]+\\):\\([0-9]+\\):?"
				      1 2) )

(setq electron-exception-line-matcher
      (list "\\(?:file:\\|    at file://\\|    at [^(]*(file://\\)\\([^:]+\\):\\([0-9]+\\)" 1 2) )

(setq rspec-line-matcher
      (list "[ \t]*# \\([^:]*\\):\\([0-9]+\\)" 1 2) )

(setq node-exception-line-matcher
      (list "[ \t]*at [^(]*(\\([^:]+\\):\\([0-9]+\\)" 1 2) )

(defvar file-line-matchers
  '(
;;    (visit-grep-n-line electron-exception-line-matcher)
;;    (visit-grep-n-line rspec-line-matcher)
    (visit-grep-n-line grep-n-matcher)
    (visit-grep-n-line python-line-matcher)
;;    (visit-java-exception-line java-exception-line-matcher)
    ) )

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
	  (apply fun args) ) ) ) )
