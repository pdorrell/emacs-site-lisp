;; Copyright (C) 2000-2013 Philip Dorrell

;;========================================================================
(setq java-word-table
      (make-alpha-table letters-digits-string "_") )

(defconst java-comment-block-line-regexp (make-regexp-old '(or "/**" "* ")))

(defun java-return ()
  "Return and indent, if inside big comment, insert a '* ' sequence"
  (interactive)
  (return-and-indent)
  (let ( (syntaxes (c-guess-basic-syntax)) (in-block-comment nil) )
    (if (and (listp syntaxes) (= (length syntaxes) 1))
	(let* ( (syntax (first syntaxes))
		(type (first syntax))
		(pos (second syntax)) )
	  (save-excursion 
	    (goto-char pos)
	    (if (looking-at java-comment-block-line-regexp)
		(setq in-block-comment t)) ) ) )
    (if in-block-comment
	(progn (insert "* ")) ) ) )
      
(defun java-identifier-at-point ()
  (word-at word-alpha-table (point)) )

(defun java-make-lower-case-var ()
  (interactive)
  (let ( (class (word-before word-alpha-table (point))) )
    (if class
	(progn
	  (insert " ")
	  (insert class)
	  (shift-initial-case) )
      (message "No word before point") ) ) )

;;-----------------------------------------------------------------
(add-hook 'java-mode-hook 'java-hook)

(defun java-hook ()
  (setq programming-language 'java)
  (local-set-key [?\C-m] 'java-return)
  (setq c-basic-offset 2)
  (local-set-key [?\C-f] 'make-for-loop)
  (local-set-key [?\C-t] 'insert-this-equals)
  (local-set-key "=" 'insert-spaced-equals)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [f10] 'shift-initial-case)
  (setq word-alpha-table java-word-table)
  (setq for-loop-variable-declarer "int")
  (setq indent-tabs-mode nil)
  (font-lock-mode 1)
  (setq comment-start "/*")
  (setq comment-end "*/")
  (setq require-final-newline t) )

(set-abbrev-language 'java)

(try-to-load "java-abbrev")

