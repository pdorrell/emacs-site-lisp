;; Copyright (C) 2000-2013 Philip Dorrell
(require 'compile)
;;========================================================================
(setq javascript-word-table
      (make-alpha-table letters-digits-string "_$") )

(setq javascript-dotted-word-table
      (make-alpha-table letters-digits-string "._$") )

(setq sgml-attribute-offset 0) ;; fix for something - used in rjsx-mode

(defun javascript-identifier-at-point ()
  (word-at word-alpha-table (point)) )

;;-----------------------------------------------------------------
(add-hook 'js-ts-mode-hook 'javascript-hook)

(defun javascript-run-file (file &rest args)
  "Run javascript FILE in node with ARGS (or some other way using :run-javascript-function project value)"
  (let ( (run-javascript-function (project-value :run-javascript-function #'nodejs-run-file)) )
    (apply run-javascript-function (cons file args)) ) )

(defconst webpack-syntax-error-line-matcher
  (list (make-regex
         '(seq start "SyntaxError:" some-whitespace (group "[^:]+") ":" 
               ".*" 
               (exact "(") (group int) ":" int (exact ")") end) )
         1 2) )

(test-regexp-list 
 webpack-syntax-error-line-matcher
 '("SyntaxError: /some/file/src/models/TodoModel.js: Unexpected token, expected \"{\" (9:42)"
    . ("/some/file/src/models/TodoModel.js" "9"))
 '("SyntaxError: /some/file/src/models/TodoModel.js: Unexpected token '(', expected \"{\" (9:42)"
    . ("/some/file/src/models/TodoModel.js" "9"))
 )

(defconst webpack-line-matchers 
  '((visit-file-at-line-number webpack-syntax-error-line-matcher))
  )

(defvar *nodejs-process* nil)

(defconst node-exception-line-matcher
  (list (make-regex
         '(seq any-whitespace "at " "[^(]*" "(" (group "[^:]+") ":" (group int))
         "[ \t]*at [^(]*(\\([^:]+\\):\\([0-9]+\\)")
        1 2) )

(defun nodejs-run-file (file &rest args)
  "Run javascript FILE in node with ARGS"
  (let ( (filename (windowize-filename (expand-file-name file))) 
	 (current-directory default-directory) )
    (let ( (javascript-executable (project-file :javascript-executable "/usr/bin/nodejs")) )
      (switch-to-buffer-other-window "*nodejs*")
      (setq file-line-matchers
	    '((visit-file-at-line-number unprefixed-grep-n-line-matcher)
	      (visit-file-at-line-number node-exception-line-matcher)))
      (setq default-directory current-directory)
      (clear-buffer)
      (stop-then-start-process "nodejs" '*nodejs-process* "*nodejs*" 
			     javascript-executable (list filename) ) ) ) )

(defvar *phantomjs-process* nil)

(defun phantomjs-run-file (file &rest args)
  "Run javascript FILE in PhantomJS with ARGS"
  (let ( (filename (windowize-filename (expand-file-name file))) 
	 (current-directory default-directory) )
    (let ( (javascript-executable (project-file :javascript-executable "/usr/bin/phantomjs"))
	   (javascript-run-script (project-file :run-javascript-file)) )
      (switch-to-buffer-other-window "*phantomjs*")
      (setq default-directory current-directory)
      (clear-buffer)
      (stop-then-start-process "phantomjs" '*phantomjs-process* "*phantomjs*" 
			     javascript-executable (list javascript-run-script filename) ) ) ) )

(defun javascript-insert-print-this-inspected ()
  "Insert console.debug for (possibly dotted) variable name just inserted"
  (interactive)
  (insert-tranformed-word 
   javascript-dotted-word-table 
   (lambda (var) (concat "console.debug(\"" var " = \", " var ");"))
   "dotted name") )

(defun javascript-hook ()
  "Hook function for javascript-mode"
  (message "javascript-hook")
  (setq programming-language 'javascript)
  (setq js-indent-level 2)
  (setq c-basic-offset 2)
  (local-set-key [?\C-f] 'make-for-loop)
  (local-set-key [?\C-t] 'insert-this-equals)
  (local-set-key "=" 'insert-spaced-equals)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key [?\C->] 'insert-function-arrow)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [f2] 'my-expand-abbrev)
  (local-set-key [f10] 'shift-initial-case)
  (local-set-key [?\C-\S-p] 'javascript-insert-print-this-inspected)
  (setq word-alpha-table javascript-word-table)
  (setq for-loop-variable-declarer "let")
  (setq run-file-function #'javascript-run-file)
  (setq indent-tabs-mode nil)
  (font-lock-mode 1)
  (setq comment-start "/*")
  (setq comment-end "*/")
  (setq *trim-trailing-whitespace-on-save* t)
  (setq require-final-newline t) )

(let ( (non-identifier-char-regex "[^A-Za-z0-9_]") )
  (set-language-search-regexes 'javascript
    :before-identifier (format "(^|%s)" non-identifier-char-regex)
    :after-identifier (format "($|%s)" non-identifier-char-regex)
    :before-definition "(function|class|async|const)\s+"
    :after-definition (format "($|%s)" non-identifier-char-regex) ) )

(set-abbrev-language 'javascript)

(load "javascript-abbrev")

;;--------------------------------------------------------------------------------
(defun insert-function-arrow() 
  (interactive)
  (just-one-space)
  (insert "=> ") )

(defun typescript-hook ()
  "Hook function for typescript-mode"
  (message "typescript-hook")
  
  (setq programming-language 'typescript)

  (setq typescript-ts-mode-indent-offset 2)
  (setq indent-tabs-mode nil)

  (setq *trim-trailing-whitespace-on-save* t)
  (setq require-final-newline t)
  
  (local-set-key [?\C-f] 'make-for-loop)
  (local-set-key [?\C-t] 'insert-this-equals)
  (local-set-key "=" 'insert-spaced-equals)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key [?\C->] 'insert-function-arrow)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [f2] 'my-expand-abbrev)
  (local-set-key [?\C-\S-p] 'javascript-insert-print-this-inspected)
  (setq word-alpha-table javascript-word-table)
  (setq for-loop-variable-declarer "let")
  (setq run-file-function #'javascript-run-file) 
  (treesit-inspect-mode) )

(add-hook 'typescript-ts-mode-hook 'typescript-hook)

(defun compile-typescript()
  (interactive)
  (compile-with-command "npm run check-code --silent" nil :typescript-base-dir) )

(let ( (non-identifier-char-regex "[^A-Za-z0-9_]") )
  (set-language-search-regexes 'typescript
    :before-identifier (format "(^|%s)" non-identifier-char-regex)
    :after-identifier (format "($|%s)" non-identifier-char-regex)
    :before-definition "(function|class|async|const|interface|type)\s+"
    :after-definition (format "($|%s)" non-identifier-char-regex) ) )

(set-abbrev-language 'typescript)

(load "typescript-abbrev")

(add-to-list 'compilation-error-regexp-alist-alist
             '(typescript
               "^\\([^(\n]+\\)(\\([0-9]+\\),\\([0-9]+\\)): \\(error\\|warning\\)"
               1 2 3 2))

(add-to-list 'compilation-error-regexp-alist 'typescript)
