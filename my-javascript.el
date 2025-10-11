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

(defun my-ts-bracketed-item-rules(parent-name)
  `( 
    (match nil ,parent-name nil nil 0)
    ((parent-is ,parent-name) first-sibling 0) 
    ) )

(my-ts-bracketed-item-rules "formal_parameters")

(defun ts-indent-end-node(end-node)
  `( ((node-is ,end-node) grand-parent 0) ) )

(defun typescript-ts-mode--indent-rules (language)
  "Rules used for indentation. Argument LANGUAGE is either `typescript' or `tsx'."
  `((,language
     ((parent-is "program") column-0 0)
     ,@(mapcan #'ts-indent-end-node `("}" ")" "]" ">"))
     ((and (parent-is "comment") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "ternary_expression") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "member_expression") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "named_imports") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "statement_block") grand-parent typescript-ts-mode-indent-offset)
     ((or (node-is "case")
          (node-is "default"))
      parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "switch_case") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "switch_default") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "type_arguments") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "type_parameters") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is ,(rx (or "variable" "lexical") "_" (or "declaration" "declarator")))
      typescript-ts-mode--anchor-decl 1)
     ((parent-is "arguments") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "array") parent-bol typescript-ts-mode-indent-offset)
     ((match nil "formal_parameters" nil nil 1) parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "formal_parameters") first-sibling 1)
     ((parent-is "template_string") no-indent) ; Don't indent the string contents.
     ((parent-is "template_substitution") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "object_pattern") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "object") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "object_type") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "enum_body") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "class_body") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "interface_body") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "arrow_function") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "binary_expression") parent-bol typescript-ts-mode-indent-offset)
     ((match "while" "do_statement") parent-bol 0)
     ((match "else" "if_statement") parent-bol 0)
     ((parent-is ,(rx (or (seq (or "if" "for" "for_in" "while" "do") "_statement")
                          "else_clause")))
      parent-bol typescript-ts-mode-indent-offset)

     ,@(when (eq language 'tsx)
	 (append (tsx-ts-mode--indent-compatibility-b893426)
		 `(((node-is "jsx_closing_element") parent 0)
		   ((match "jsx_element" "statement") parent typescript-ts-mode-indent-offset)
		   ((parent-is "jsx_element") parent typescript-ts-mode-indent-offset)
		   ((parent-is "jsx_text") parent-bol typescript-ts-mode-indent-offset)
		   ((parent-is "jsx_opening_element") parent typescript-ts-mode-indent-offset)
		   ((parent-is "jsx_expression") parent-bol typescript-ts-mode-indent-offset)
		   ((match "/" "jsx_self_closing_element") parent 0)
		   ((parent-is "jsx_self_closing_element") parent typescript-ts-mode-indent-offset))))
     ;; FIXME(Theo): This no-node catch-all should be removed.  When is it needed?
     (no-node parent-bol 0))) )

(typescript-ts-mode--indent-rules 'typescript)


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
  (setq run-file-function #'javascript-run-file) )

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
