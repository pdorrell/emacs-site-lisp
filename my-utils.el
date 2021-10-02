;; Copyright (C) 2000,2001 Philip Dorrell

(defmacro cons-bind (var1 var2 expression &rest statements)
  "Bind car and cdr of EXPRESSION to VAR1 and VAR2 in STATEMENTS"
  (declare (indent defun))
  `(cl-destructuring-bind (,var1 . ,var2) ,expression
     ,@statements) )

;  Possibly not useful ...
(defun apply-to-list-of-arg-lists (fun arg-lists)
  (declare (indent defun))
  (dolist (arg-list
           arg-lists)
    (apply fun arg-list) ) )
  
(defun check-count (args n)
  (if (not (eql (length args) 1))
      (error "Expecting %s arguments" n) ) )

(defun only-arg (args)
  (check-count args 1)
  (car args) )

(defun make-postfix-regexp (regexp suffix)
  (concat regexp suffix) )

(defun make-seq-regexp (args)
  (apply #'concat (mapcar #'make-regexp-old args)) )

;;--------------------------------------------------------------------------------
(defun interpreter-error (lookup message expression)
  (error "%s: ERROR %s in expression %S" 
         (funcall lookup :name)
         message expression) )

(defun interpret (lookup expr)
  (cond
   ((null expr)
    (lookup :nil nil))
   ((listp expr)
    (cons-bind function-name args expr
      (if (eq function-name 'quote)
          (if (eq (length args) 1)
              (first args)
            (interpreter-error lookup "quote requires 1 argument" expr) )
        (let ( (fun (funcall lookup function-name)) )
          (if (null fun)
              (interpreter-error lookup (format "function %s not defined" function-name) expr) )
          (apply fun (mapcar (lambda (arg) (interpret lookup arg)) args)) ) ) ) )
   ((symbolp expr)
    (let ( (value (funcall lookup expr)) )
      (if (null value)
          (interpreter-error lookup "symbol not defined" expr) )
      value))
   (t expr) ) )
           
;;--------------------------------------------------------------------------------

(defconst *make-regexp-interpreter-lookups-alist*
  (list
   (cons :name "make-regexp")
   (cons 'exact #'(lambda (value) (regexp-quote value)))
   (cons 'seq #'concat)
   (cons 'group #'(lambda (&rest values) (concat "\\(" (string-join values "\\|") "\\)")))
   (cons 'shy-group #'(lambda (&rest values) (concat "\\(?:" (string-join values "\\|") "\\)")))

   (cons 'int "[0-9]+")
   (cons 'any-whitespace "[ \t]*")
   (cons 'some-whitespace "[ \t]+")
   (cons 'some-non-whitespace "[^ \t]+")
   ) )

(defun make-regexp-interpreter-lookup (symbol &optional default)
  (let ( (item (assoc symbol *make-regexp-interpreter-lookups-alist*)) )
    (if item (cdr item)) ) )

(funcall (make-regexp-interpreter-lookup 'group) "jim" "tom")

(defun make-regexp-new (expr)
  (interpret #'make-regexp-interpreter-lookup expr) )

(run-test (make-regexp-new '(seq "jim" "tom")) "jimtom")

(run-test 
  (make-regexp-new '(group (seq "abc" "[a-z]" "def" int) "something"))
  "\\(abc[a-z]def[0-9]+\\|something\\)")

;;--------------------------------------------------------------------------------

(defun make-regexp-old (expr)
  (cond
    ((stringp expr) (regexp-quote expr))
    ((listp expr)
     (let ( (fun (car expr))
	    (args (cdr expr)) )
       (case fun
	 (paren (concat "\\(" (make-seq-regexp args) "\\)"))
	 (seq (make-seq-regexp args))
	 (set (concat "[" (only-arg args) "]"))
	 (quote (only-arg args))
	 (repeated (make-postfix-regexp (make-regexp-old (only-arg args)) "*"))
	 (at-least-once (make-postfix-regexp (make-regexp-old (only-arg args)) "+"))
	 (maybe (make-postfix-regexp (make-regexp-old (only-arg args)) "?"))
	 (not-set (concat "[^" (only-arg args) "]"))
	 (or (if (< (length args) 1) (error "Expect at least one argument"))
	     (let ( (result (make-regexp-old (car args))) )
	       (dolist (arg (cdr args))
		 (setq result (concat result "\\|" (make-regexp-old arg))) )
	       result) )
	 (otherwise (error "Unknown regexp function %S" fun)) ) ) )
    ((symbolp expr)
     (case expr
       (start "^")
       (end "$")
       (buffer-start "\\`")
       (buffer-end "\\'")
       (any ".")
       (t (symbol-value expr)) ) ) ) )

(defun make-regexp-new-and-old (expr-new expr-old)
  (let ( (old-regexp (make-regexp-old expr-old))
         (new-regexp (make-regexp-new expr-new)) )
    (if (not (equal old-regexp new-regexp))
      (error "New regex %S from %S != Old regex %S from %S"
             new-regexp expr-new old-regexp expr-old))
    new-regexp) )

;;--------------------------------------------------------------------------------
(defun match-regexp-list-in-string (regexp-list string)
  "Match REGEXP-LIST consisting of a regex and then a list of group numbers, 
  against STRING, return nil if no match, else the list of matched substrings for the group numbers"
  (let ( (regexp (car regexp-list))
	 (positions (cdr regexp-list)) matchdata)
    (if (string-match regexp string)
        (let ( (matchdata (match-data t)) (result nil) )
	  (dolist (position positions)
	    (let* ( (startpos (* position 2))
		    (endpos (1+ startpos)) )
	      (setq result (cons (substring string (elt matchdata startpos)
						   (elt matchdata endpos))
				 result)) ) )
	  (reverse result) ) ) ) )

(defun match-regexp (regexp-list)
  "Match REGEXP-LIST consisting of a regex and then a list of group numbers, 
   against current buffer line, return nil if no match, else the list of matched substrings for the group numbers"
  (match-regexp-list-in-string regexp-list (buffer-line (point))) )

(defun test-regexp-list (regexp-list &rest string-result-pairs)
  (dolist (string-result string-result-pairs)
    (cons-bind string result string-result
      (run-test
        (match-regexp-list-in-string regexp-list string)
        result) ) )
  (message "test-regexp-list all passed") )

(test-regexp-list
 '("^\\([0-9]+\\)jim\\([a-z]*\\)$" 1 2)
 '("1235jimmy" . ("1235" "my"))
 '(" 1234jimmy" . nil) )

(defun nth-last-pos (string ch n)
  (let* ( (len (length string)) 
	  (pos (1- len))
	  (count 0) )
    (block nil
      (while (> pos 0)
	(if (eql ch (aref string pos))
	    (setq count (1+ count)) )
	(if (eql count n)
	    (return pos) )
	(setq pos (1- pos)) )
      nil) ) )

(defun replace-char (string old new)
  "In STRING, replace occurrences of OLD character with NEW character"
  (let ( (result (copy-seq string))
	 (len (length string)) )
    (dotimes (i len)
      (if (eql (aref string i) old)
	  (aset result i new) ) )
    result) )

(defun capitalize-initial-letter (string)
  "Capitalize initial letter of STRING"
  (let ( (len (length string)) )
    (if (= 0 len)
	string
      (let* ( (ch (aref string 0)) )
	(if (and (>= ch ?a) (<= ch ?z))
	    (let ( (new-string (copy-sequence string)) )
	      (aset new-string 0 (+ ch (- ?A ?a)))
	      new-string)
	  string) ) ) ) )

(defun string-starts-with (string pat)
  (let ( (string-len (length string))
	 (pat-len (length pat)) )
    (if (<= pat-len string-len)
	(equal (substring string 0 pat-len) pat)
      nil) ) )

(defun string-ends-with (string pat)
  (let ( (string-len (length string))
	 (pat-len (length pat)) )
    (if (<= pat-len string-len)
	(equal (substring string (- string-len pat-len) string-len) pat)
      nil) ) )

(defun string-to-lower-case (string)
  (let* ( (result (copy-sequence string))
	  (len (length string))
	  (offset (- ?a ?A)) )
    (dotimes (i len)
      (let ( (ch (aref string i)) )
	(if (and (<= ?A ch) (<= ch ?Z))
	    (aset result i (+ offset ch)) ) ) )
    result) )

(defun starts-upper-case (string)
  (let ( (ch (aref string 0)) )
    (and (>= ch ?A) (<= ch ?Z)) ) )

(defun strip-whitespace (string)
  (let* ( (result (copy-sequence string))
	  (i 0) (j 0)
	  (len (length string)) ch)
    (dotimes (i len)
      (setq ch (aref string i))
      (if (not (or (= ch ? ) (= ch ?	)))
	  (progn
	    (aset result j ch)
	    (setq j (1+ j)) ) ) )
    (substring result 0 j) ) )

(defun separated-values (values separator)
  (let ( (all-values nil) )
    (dolist (value values)
      (setq all-values (cons value all-values))
      (setq all-values (cons separator all-values)) )
    (if all-values
	(setq all-values (cdr all-values)) )
    (apply 'concat (reverse all-values)) ) )

(defun file-name-minus-extension (file-name)
  (block nil 
    (let ( (pos (1- (length file-name))) )
      (while (>= pos 0)
	(if (= (aref file-name pos) ?.)
	    (return (substring file-name 0 pos))
	  (setq pos (1- pos)) ) ) )
    file-name) )

(defun buffer-line (pos)
  "Get contents of line in current buffer at specified position POS"
  (save-excursion
    (goto-char pos)
    (let ( start-pos end-pos)
      (beginning-of-line)
      (setq start-pos (point))
      (end-of-line)
      (setq end-pos (point))
      (buffer-substring-no-properties start-pos end-pos) ) ) )

;;(buffer-line (point))

(defun listify-if-not-list (value)
  (if (listp value) value (list value)) )

(defun first-element-if-list(value)
  (if (listp value) (car value) value) )

(defmacro getting-value (&rest statements)
  "A block of STATEMENTS from which a value is returned using return-value"
  (declare (indent defun))
  `(block 'value ,@statements) )

(defmacro return-value (expression)
  "Python-style 'return': Return EXPRESSION as a value to a block tagged with 'value, eg using getting-value or defun-getting-value"
  `(cl-return-from 'value ,expression) )

(defmacro defun-getting-value (name args &rest body)
  "Define function NAME with ARGS and BODY that returns a value using return-value"
  (declare (indent defun))
  `(defun ,name ,args
     (getting-value ,@body) ) )

(setq *test-files-directory* (expand-file-name "test" emacs-customisation-dir))

(defun test-file (file-name)
  (expand-file-name file-name *test-files-directory*) )

