
(defun make-sql-startup-command ()
  (list (expand-file-name "bin/run-sql-client" emacs-customisation-dir)) )

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

(defun sqlite-set-connection (url)
  (sql-set-connection url "org.sqlite.JDBC") )

(defun hsqldb-set-connection (url &optional user password)
  (sql-set-connection url "org.hsqldb.jdbcDriver" user password) )

(defun sql-execute-query (query)
  (sql-command
   (list "executeQuery" query) ) )

(defconst sql-start-regexp
  (make-regex '(one-of buffer-start (seq ";" end) blank-line)) )

(defconst sql-end-regexp
  (make-regex '(one-of buffer-end (seq ";" end) blank-line)) )


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
  (setq programming-language 'sql)
  (font-lock-mode 1)
  (local-set-key [f2] 'my-expand-abbrev)
  )

(add-hook 'sql-mode-hook 'sql-hook-function)

(defun visit-sql-file()
  (interactive)
  (find-file sql-file) )

(set-abbrev-language 'sql)

(set-abbrevs 
 'sql
 '( 
   ("s" ("select " mark " from " goto-mark))
   ("sf" "select * from ")
   ("sfw" ("select * from " mark " where " goto-mark))
   ("d" ("delete from " mark " where " goto-mark))
   ("i" "insert into ")
   ("v" ("values (" mark ")" goto-mark))
   ("u" "update ")
   ("us" ("update " mark " set " return "  where" goto-mark))
   ("w" "where ")
   ("g" "group by ")
   ("o" "order by ") ) )

