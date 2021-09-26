;; Copyright (C) 2000,2001 Philip Dorrell

;;========================================================================
(defun set-abbrev-language (language)
  "Set LANGUAGE as a language for which abbreviations can be defined"
  (if (not (get 'my-abbrevs-by-language language))
      (put 'my-abbrevs-by-language language (make-hash-table :test 'equal)) ) )

(defun get-abbrevs-for-language(language)
  "Get the hashtable of abbreviations for LANGUAGE"
  (let ( (language-abbrevs (get 'my-abbrevs-by-language language)) )
    (if language-abbrevs
        language-abbrevs
      (error "Unexpected abbrev language %s" language) ) ) )

(defun get-abbrev (abbrev)
  "Expand ABBREV using the language for buffer-local variable programming-language"
  (let ( (language-abbrevs (get-abbrevs-for-language programming-language)) )
    (gethash abbrev language-abbrevs) ) )

(defun set-abbrevs (language abbrev-expansion-pairs)
  "Set abbreviations for LANGUAGE from list of pairs of abbreviations & expansions"
  (let ( (language-abbrevs (get 'my-abbrevs-by-language language)) )
    (dolist (abbrev-expansion abbrev-expansion-pairs)
      (cl-destructuring-bind (abbrev expansion) abbrev-expansion
        (puthash abbrev expansion language-abbrevs) ) ) ) )

(defvar abbrev-word-table nil 
  "The alphabet table used to identify an abbreviation just before the current point")
(make-variable-buffer-local 'abbrev-word-table)

(setq-default abbrev-word-table (make-alpha-table
				 "abcdefghijklmnopqrstuvwxyz") )

(defun get-abbrev-before()
  "Get the abbreviation before the current point, ie (usually) what the user has just typed"
   (let ((start (1- (point))) (end (point)) )
    (while (buffer-char-in-table abbrev-word-table start)
      (setq start (1- start)) )
    (setq start (1+ start))
    (if (> end start)
	(buffer-substring-no-properties start end)
      nil) ) )

(defun my-expand-abbrev ()
  "Expand abbreviation defined using get-abbrev and programming-language. 
   An abbreviation may map to a list of instructions, or if may map simply to a longer string"
  (interactive)
  (let ( (abbrev (get-abbrev-before)) expansion)
    (if abbrev
	(progn
	  (setq expansion (get-abbrev abbrev))
	  (if expansion
	      (progn 
		(delete-backward-char (length abbrev))
		(if (listp expansion)
		    (complex-expand expansion)
		  (insert expansion) ) )
	    (message 
	     (concat "No expansion for abbreviation \"" 
		     abbrev "\"") ) ) )
      (message "No abbreviation before point") ) ) )

(defun complex-expand (expansion)
  "Expand an abbreviation that maps to a list of instructions to be executed. 
   Each instruction can be a string to be inserved, and function to be called,
   or a function to be called with some arguments. Functions are represented by
   symbols that are properties of the symbol 'abbrev-fun"
  (let ( (mark (point)) )
    (dolist (item expansion)
      (if (stringp item)
	  (insert item)
	(if (symbolp item)
	    (call-abbrev-fun item nil)
	  (call-abbrev-fun (car item) (cdr item)) ) ) ) ) )

(defun call-abbrev-fun (name args)
  "Call the named abbreviation function with a list of arguments"
  (let ( (fun (get 'abbrev-fun name)) )
    (if fun
	(apply fun args)
      (error "Undefined abbreviation function: ~A" name) ) ) )

(defmacro def-abbrev-fun (name args &rest code)
  "Define the abbreviation function with arguments and code"
  `(put 'abbrev-fun ',name #'(lambda ,args ,@code)) )

(def-abbrev-fun mark ()
  "Remember current position into variable abbrev-mark"
  (setq abbrev-mark (point)) )

(def-abbrev-fun goto-mark ()
  "Go to previously remembered position in abbrev-mark"
  (goto-char abbrev-mark) )

(def-abbrev-fun indent ()
  "Indent this line"
  (indent-for-tab-command) )

(def-abbrev-fun return ()
  "Start a new line"
  (insert "\n") )

(def-abbrev-fun space-if-not-there ()
  "Insert space if not already there"
  (insert-space-if-not-there) )

(def-abbrev-fun base-name ()
  "Get base name of file"
  (insert (file-name-minus-extension (buffer-name))) )

(def-abbrev-fun thick-comment-line ()
  "Insert a thick comment line"
  (thick-comment-line) )

(def-abbrev-fun file (name &optional default-value)
  "Insert contents of named file, and option default-value if the named file does not exist"
  (if (file-exists-p name)
      (forward-char (second (insert-file-contents name)))
    (if default-value
	(insert default-value)
      (insert (concat comment-start " No file " name " " comment-end "\n")) ) ) )

(def-abbrev-fun java-before-package ())
(def-abbrev-fun java-after-package ())

(def-abbrev-fun java-package ()
  (insert "package " (java-get-package) ";\n") )

(def-abbrev-fun new-line-before ()
  "Insert a new line before this line"
  (let ( empty-line )
    (save-excursion
      (beginning-of-line)
      (setq empty-line (looking-at "[ \t]*$") ) )
    (if (not empty-line)
	(insert "\n")) ) )
