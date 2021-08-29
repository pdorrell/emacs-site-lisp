;; Copyright (C) 2000,2001 Philip Dorrell

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
  (apply #'concat (mapcar #'make-regexp args)) )

(defun make-regexp (expr)
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
	 (repeated (make-postfix-regexp (make-regexp (only-arg args)) "*"))
	 (at-least-once (make-postfix-regexp (make-regexp (only-arg args)) "+"))
	 (maybe (make-postfix-regexp (make-regexp (only-arg args)) "?"))
	 (not-set (concat "[^" (only-arg args) "]"))
	 (or (if (< (length args) 1) (error "Expect at least one argument"))
	     (let ( (result (make-regexp (car args))) )
	       (dolist (arg (cdr args))
		 (setq result (concat result "\\|" (make-regexp arg))) )
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

(defun match-regexp (regexp-list)
  (let ( (regexp (car regexp-list))
	 (positions (cdr regexp-list)) matchdata)
    (if (looking-at regexp)
	(let ( (result nil) )
	  (setq matchdata (match-data t))
	  (dolist (position positions)
	    (let* ( (startpos (* position 2))
		    (endpos (1+ startpos)) )
	      (setq result (cons (buffer-substring (elt matchdata startpos)
						   (elt matchdata endpos))
				 result)) ) )
	  (reverse result) ) ) ) )

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
  "Get contents of line in current buffer at specified position POS"
  (buffer-substring-no-properties 
   (line-start pos) (line-end pos) ) )
