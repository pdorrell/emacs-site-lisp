;-----------------------------------------------------------------
(defun char-not-eoln (ch)
  (and ch (/= ch 10)) )

(defun line-start (pos)
  "Start of line that pos is in"
  (let ( (start (1- pos)) )
    (while (char-not-eoln (char-after start))
      (setq start (1- start)) )
    (setq start (1+ start)) 
    start) )

(defun line-end (pos)
  "End of line that pos is in"
  (let ( (end pos) )
    (while (char-not-eoln (char-after end))
      (setq end (1+ end)) )
    end) )

(defun buffer-line (pos)
  (buffer-substring-no-properties 
   (line-start pos) (line-end pos) ) )

(setq python-line-matcher 
      (list "[ ]*File[ ]*\"\\([^\"]*\\)\",[ ]*line[ ]*\\([0-9]+\\)"
	    1 2) )

(defun visit-project-file-grep-n-line (file-name line-number-string)
  (let ( (full-file-name (expand-file-name file-name (project-base-directory)))
	 (line-number (string-to-number line-number-string)) )
    ;; (message "visiting file %s line %s ..." full-file-name line-number)
    (find-file full-file-name)
    (goto-line line-number) ) )

(defun visit-grep-n-line (file-name line-number-string)
  (let ( (line-number (string-to-number line-number-string)) )
    ;; (message "visiting file \"%s\" line %s ..." file-name line-number)
    (find-file file-name)
    (goto-line line-number)
    ) )

;; [ \\t\\n]*\\(from \\)?
(setq grep-n-matcher (list "[ \t]*\\(?:from \\|\\)\\([a-zA-Z]?[:]?[^:]+\\):\\([0-9]+\\):?"
			   1 2) )

(setq unprefixed-grep-n-matcher (list "^\\([a-zA-Z]?[:]?[^: ]+\\):\\([0-9]+\\):?"
				      1 2) )

(setq electron-exception-line-matcher
      (list "\\(?:file:\\|    at file://\\|    at [^(]*(file://\\)\\([^:]+\\):\\([0-9]+\\)" 1 2) )

(setq rspec-line-matcher
      (list "[ \t]*# \\([^:]*\\):\\([0-9]+\\)" 1 2) )

(setq node-exception-line-matcher
      (list "[ \t]*at [^(]*(\\([^:]+\\):\\([0-9]+\\)" 1 2) )

(setq java-exception-line-matcher
      (list (make-regexp '(seq (maybe "+") (at-least-once (set " \t")) "at " 
			       (paren (at-least-once (set "a-zA-Z0-9._$<>")))
			       "(" (paren (at-least-once (set "a-zA-Z0-9_"))) ".java:"
			       (paren (at-least-once (set "0-9"))) ")"))
	    1 2 3) )

(defun visit-java-exception-line (method-name class-name line-number-string)
  (let* ( (line-number (string-to-number line-number-string)) 
	  (package-name (package-from-method-name method-name))
	  (source-file (find-java-source-file (list package-name) class-name)) )
    (if source-file
	(progn
	  (find-file source-file)
	  (goto-line line-number) )
      (message "Cannot find java file %s.java in package %s" class-name package-name) ) ) )
	  

(defun package-from-method-name (method-name)
  (let ( (package-end (nth-last-pos method-name ?. 2)) )
    (if package-end
	(substring method-name 0 package-end)
      "") ) )

(defvar file-line-matchers
  '((visit-grep-n-line electron-exception-line-matcher)
    (visit-grep-n-line rspec-line-matcher)
    (visit-grep-n-line grep-n-matcher)
    (visit-grep-n-line python-line-matcher)
    (visit-java-exception-line java-exception-line-matcher)) )

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
