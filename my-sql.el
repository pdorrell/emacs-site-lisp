
(defvar *jdbc-drivers-classpath* "" "Classpath for JDBC drivers")

(defun make-sql-startup-command ()
  (list *java-executable* "-cp" (concat emacs-util-classpath ";" *jdbc-drivers-classpath*) 
	"com1729.emacs.sql.Sql") )

(defun sql-command (command)
  (if (not (equal (buffer-name) "*sql*"))
      (switch-to-buffer-other-window "*sql*") )
  (goto-char (point-max))
  (let ( (sql-lines (cons (number-to-string (length command)) command)) )
    (run-process-with-lines "sql" (make-sql-startup-command) sql-lines) ) )

(defun sql-set-divider (divider)
  (sql-command
   (list "setDivider" divider)) )

(defun sql-set-connection (url driver &optional user password)
  (message "driver=%s" driver)
  (sql-command
   (if user
       (list "setConnectionWithUserPassword" url driver user (if password password ""))
     (list "setConnection" url driver) ) ) )

(defun sql-set-show-types (show-types)
  (sql-command
   (list "setShowTypes" (if show-types "true" "false")) ) )

(defun sql-set-show-quotes (show-quotes)
  (sql-command
   (list "setShowQuotes" (if show-quotes "true" "false")) ) )
  

(defun sql-disconnect ()
  (sql-command
   (list "disconnect") ) )

(defun mysql-set-connection (url &optional user password)
  (sql-set-connection url "com.mysql.jdbc.Driver" user password) )

(defun hsqldb-set-connection (url &optional user password)
  (sql-set-connection url "org.hsqldb.jdbcDriver" user password) )

(defun mysql-set-connection (url &optional user password)
  (sql-set-connection url "com.mysql.jdbc.Driver" user password) )

(defun sql-execute-query (query)
  (sql-command
   (list "executeQuery" query) ) )

(defvar sql-start-regexp
  (make-regexp '(or buffer-start (seq ";" end) 
		    (seq start (paren (repeated (set " \t")))
			 (paren (or (paren (seq (set "gG") (set "oO"))) (paren "")))
			 (paren (repeated (set " \t"))) end) ) ) )

(defvar sql-end-regexp
  (make-regexp '(or buffer-end (seq ";" end) 
		    (seq start (paren (repeated (set " \t")))
			 (paren (or (paren (seq (set "gG") (set "oO"))) (paren "")))
			 (paren (repeated (set " \t"))) end) ) ) )


(defun sql-get-delimited-query ()
  (let ( query-start query-end )
    (save-excursion
      (beginning-of-line)
      (save-excursion
	(search-backward-regexp sql-start-regexp)
	(setq query-start (match-end 0)) )
      (save-excursion
	(search-forward-regexp sql-end-regexp)
	(setq query-end (match-beginning 0)) )
      (buffer-substring query-start query-end) ) ) )

(defun sql-execute-query-at-point ()
  (interactive)
  (save-this-buffer-and-others)
  (let ( (query
	  (if saved-mouse-selection
	      (get-saved-mouse-selection) 
	    (sql-get-delimited-query) ) ) )
    (setq query (replace-char query ?\n ? ))
    (if (or (eq major-mode 'sql-mode)
	    (y-or-n-p (concat "Really execute SQL: \"" query "\" ?")))
	(progn
	  (message "Executing SQL: %s" query)
	  (sql-execute-query query) )
      (message "Query not executed") ) ) )

(defun sql-set-max-rows (n)
  (interactive)
  (sql-command (list "setMaxRows" (number-to-string n))) )

(defun sql-set-page-length (n)
  (interactive)
  (sql-command (list "setPageLength" (number-to-string n))) )

(setq sql-mode-hook '(sql-hook))

(eval-expression "(+ 2 3)")

(defun sql-hook ()
  (local-set-key [f9] 'sql-eval-lisp-in-comment)
  )

(defun sql-eval-lisp-in-comment ()
  (interactive)
  (let ( start end )
    (save-excursion
      (beginning-of-line)
      (if (looking-at "#")
	  (forward-char 1) )
      (setq start (point))
      (end-of-line)
      (setq end (point)) )
    (eval-region start end t) ) )

(defvar sql-file nil "SQL file visited by visit-sql-file")

(defun sql-hook-function() 
  (setq expansion-key 'sql-expansion-key)
  (font-lock-mode 1)
  (local-set-key [f2] 'my-expand-abbrev)
  )

(defun def-sql-abbrev (abbrev expansion)
  "Define ABBREV to have EXPANSION in SQL mode"
  (set-abbrev 'sql-expansion-key abbrev expansion) )

(add-hook 'sql-mode-hook 'sql-hook-function)

(defun visit-sql-file()
  (interactive)
  (find-file sql-file) )

(def-sql-abbrev "s" '("select " mark " from " goto-mark))
(def-sql-abbrev "sf" "select * from ")
(def-sql-abbrev "sfw" '("select * from " mark " where " goto-mark))
(def-sql-abbrev "d" '("delete from " mark " where " goto-mark))
(def-sql-abbrev "i" "insert into ")
(def-sql-abbrev "v" '("values (" mark ")" goto-mark))
(def-sql-abbrev "u" "update ")
(def-sql-abbrev "us" '("update " mark " set " return "  where" goto-mark))
(def-sql-abbrev "w" "where ")
(def-sql-abbrev "g" "group by ")
(def-sql-abbrev "o" "order by ")
