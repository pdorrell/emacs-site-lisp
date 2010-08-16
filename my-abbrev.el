;; Copyright (C) 2000,2001 Philip Dorrell

;;========================================================================
(defun set-abbrev (abbrev expansion key)
  (put (intern abbrev) key expansion) )

(make-variable-buffer-local 'expansion-key)

(make-variable-buffer-local 'abbrev-word-table)

(setq-default abbrev-word-table (make-alpha-table
				 "abcdefghijklmnopqrstuvwxyz") )

(defun get-abbrev-before()
   (let ((start (1- (point))) (end (point)) )
    (while (buffer-char-in-table abbrev-word-table start)
      (setq start (1- start)) )
    (setq start (1+ start))
    (if (> end start)
	(buffer-substring-no-properties start end)
      nil) ) )

(defun get-abbrev (abbrev)
  (get (intern abbrev) expansion-key) )

(defun my-expand-abbrev ()
  "Expand abbreviation defined using set-abbrev and expansion-key"
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
  (let ( (mark (point)) )
    (dolist (item expansion)
      (if (stringp item)
	  (insert item)
	(if (symbolp item)
	    (call-abbrev-fun item nil)
	  (call-abbrev-fun (car item) (cdr item)) ) ) ) ) )

(defun call-abbrev-fun (name args)
  (let ( (fun (get name 'abbrev-fun)) )
    (if fun
	(apply fun args)
      (error "Undefined abbreviation function: ~A" name) ) ) )

(defmacro def-abbrev-fun (name args &rest code)
  `(put ',name 'abbrev-fun #'(lambda ,args ,@code)) )

(def-abbrev-fun mark ()
  (setq abbrev-mark (point)) )

(def-abbrev-fun goto-mark ()
  (goto-char abbrev-mark) )

(def-abbrev-fun indent ()
  (indent-for-tab-command) )

(def-abbrev-fun return ()
  (insert "\n") )

(def-abbrev-fun space-if-not-there ()
  (insert-space-if-not-there) )

(defun file-name-minus-extension (file-name)
  (block nil 
    (let ( (pos (1- (length file-name))) )
      (while (>= pos 0)
	(if (= (aref file-name pos) ?.)
	    (return (substring file-name 0 pos))
	  (setq pos (1- pos)) ) ) )
    file-name) )

(def-abbrev-fun base-name ()
  (insert (file-name-minus-extension (buffer-name))) )

(def-abbrev-fun thick-comment-line ()
  (thick-comment-line) )

(def-abbrev-fun copyright ()
  (insert (concat comment-start copyright-line comment-end "\n") ) )

(def-abbrev-fun file (name &optional default-value)
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
  (let ( empty-line )
    (save-excursion
      (beginning-of-line)
      (setq empty-line (looking-at "[ \t]*$") ) )
    (if (not empty-line)
	(insert "\n")) ) )
