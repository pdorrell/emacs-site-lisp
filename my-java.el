;; Copyright (C) 2000-2013 Philip Dorrell

;;========================================================================
(setq java-word-table
      (make-alpha-table letters-digits-string "_") )

(make-variable-buffer-local 'word-alpha-table)
(make-variable-buffer-local 'for-loop-variable-declarer)

(defun make-for-loop ()
  "Make for loop using preceding variable name"
  (interactive)
  (let ( (var (word-before word-alpha-table (point)))
	 saved-point)
    (if var
	(progn
	  (delete-backward-char (length var))
	  (insert "for (" for-loop-variable-declarer " " var "=0; " var "<")
	  (setq saved-point (point))
	  (insert "; " var "++) {\n")
	  (indent-for-tab-command)
	  (insert "\n}") (indent-for-tab-command)
	  (goto-char saved-point) )
      (message "No variable name given for for loop") ) ) )

(defun insert-this-equals ()
  "Do this.x=x on preceding x"
  (interactive)
  (let* ( (var (word-before word-alpha-table (point)))
	  (member-var var) )
    (if var
	(progn
	  (delete-backward-char (length var))
	  (insert "this." member-var " = " var ";") )
      (message "No variable name given") ) ) )

(setq equals-op-table (make-alpha-table "+-=/*&|^%<>!"))

(defun insert-routine-args ()
  "Insert space, brackets and braces"
  (interactive)
  (let ( (pos) )
    (insert-space-if-not-there)
    (insert "(") (setq pos (point))
    (insert ") {\n")
    (indent-for-tab-command)
    (insert "\n}") (indent-for-tab-command)
    (goto-char pos) ) )

(defun java-parameter-var (member-var)
  member-var)

(defun toggle-private ()
  "Make variable or routine private, or not private"
  (interactive)
    (beginning-of-line)
    (if (looking-at "[ ]*[a-zA-Z_]")
	(progn
	  (search-forward-regexp "[^ ]")
	  (backward-char)
	  (cond
	   ((looking-at "private") (delete-char 7))
	   ((looking-at "protected") (delete-char 9) (insert "private"))
	   ((looking-at "public") (delete-char 6) (insert "private"))
	   (t (insert "private ")) )
	  (indent-for-tab-command)
	  (beginning-of-line)
	  (forward-line) )
      (error "Nothing to toggle private on") ) )

(setq java-type-regexp (make-regexp '(at-least-once (set "][a-zA-Z0-9_."))))
(setq java-variable-regexp (make-regexp '(at-least-once (set "a-zA-Z0-9_"))))
(setq java-some-whitespace-regexp (make-regexp '(at-least-once (set " \t"))))
(setq java-maybe-whitespace-regexp (make-regexp '(repeated (set " \t"))))
(setq java-array-regexp (make-regexp '(repeated (set "]["))))

(setq java-var-declaration-matcher
      (list (make-regexp '(seq java-maybe-whitespace-regexp
			       (paren (or (seq '"\\b" "private" '"\\b" java-some-whitespace-regexp) ""))
			       (paren java-type-regexp)
			       java-some-whitespace-regexp
			       (paren java-variable-regexp)
			       (paren java-array-regexp)
			       ) )
	    2 3 4) )

(defun java-insert-getter (type var array)
  (let ( (full-type (concat type array)) )
    (insert "public " full-type " get" (capitalize-initial-letter var) "() {") (end-of-line)
    (indent-for-tab-command) (return-and-indent)
    (insert "return " var ";") (end-of-line) (return-and-indent)
    (insert "}") (indent-for-tab-command) ) )
			       
(defun java-insert-setter (type var array)
  (let ( (full-type (concat type array))  
	 (param-var (java-parameter-var var)) )
    (insert "public void set" (capitalize-initial-letter var) " (" full-type " " param-var ") {") (end-of-line)
    (indent-for-tab-command) (return-and-indent)
    (insert "this." var " = " param-var ";") (end-of-line) (return-and-indent)
    (insert "}") (indent-for-tab-command) ) )
			       
(defun java-make-getter ()
  "Convert variable definition line into getter"
  (interactive)
  (beginning-of-line)
  (if (looking-at "^[ \t]*$") (next-line -1))
  (let ( (var-data (match-regexp java-var-declaration-matcher)) )
    (if var-data
	(progn 
	  (kill-line nil)
	  (apply #'java-insert-getter var-data) )
      (message "No match") ) ) )

(defun java-make-setter ()
  "Convert variable definition line into getter"
  (interactive)
  (beginning-of-line)
  (if (looking-at "^[ \t]*$") (next-line -1))
  (let ( (var-data (match-regexp java-var-declaration-matcher)) )
    (if var-data
	(progn 
	  (kill-line nil)
	  (apply #'java-insert-setter var-data) )
      (message "No match") ) ) )

(defun java-make-setter-and-getter ()
  "Convert variable definition line into getter"
  (interactive)
  (beginning-of-line)
  (if (looking-at "^[ \t]*$") (next-line -1))
  (let ( (var-data (match-regexp java-var-declaration-matcher)) )
    (if var-data
	(progn 
	  (kill-line nil)
	  (apply #'java-insert-setter var-data)
	  (return-and-indent) (return-and-indent)
	  (apply #'java-insert-getter var-data) )
      (message "No match") ) ) )

(setq java-comment-block-line-regexp (make-regexp '(or "/**" "* ")))

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
      
;;-----------------------------------------------------------------

(setq java-filter-regexp
      (make-regexp '(or (paren (at-least-once (set "a-zA-Z")))
			(paren (seq (set " \t") (maybe (set " "))
				    (paren (or (paren (at-least-once (set "a-oq-zA-Z")))
					       (seq (repeated (set " \t")) "p" (paren (or "ublic " 
								   (paren (seq "r" 
									       (paren (or "otected " "ivate")) )) ) ) )
					       )) )) )) )

(defun get-java-path (tag)
  (let ( (path nil) lib src-dir)
    (dolist (lib-name (project-value :java-libraries))
      (setq lib (symbol-value lib-name))
      (setq path-list (cdr (assoc tag lib)))
      (setq path (append (reverse path-list) path)) )
    (reverse path) ) )

(defun get-java-source-path ()
  (get-java-path 'src) )

(defun get-java-javadoc-path ()
  (get-java-path 'javadoc) )

(defun get-java-classpath ()
  (separated-values (get-java-path 'classpath) ";") )

(defun get-java-class-names (packages base-name)
  (let ( (class-names nil) )
    (dolist (package packages)
      (setq class-names (cons (concat package "." base-name) class-names)) )
    (reverse class-names) ) )

(defun dots-to-slashes (dotted-name)
  (replace-char dotted-name ?. ?/) )

(defun class-name-to-url (class-name)
  (concat (dots-to-slashes class-name) ".html") )

(defun find-javadoc-url (packages base-name)
  (let ( (url-names (mapcar #'class-name-to-url (get-java-class-names packages base-name)))
	 (java-javadoc-path (get-java-javadoc-path)) )
    (block loop
      (dolist (dir java-javadoc-path)
	(dolist (url-name url-names)
	  (let ( (full-url (concat dir url-name)) )
	    (if (file-exists-p full-url)
		(return-from loop full-url) ) ) ) ) ) ) )

(defun class-name-to-source-file (class-name)
  (concat (dots-to-slashes class-name) ".java") )

(defun find-java-source-file (packages base-name)
  (let ( (source-file-names (mapcar #'class-name-to-source-file 
				    (get-java-class-names packages base-name)))
	 (java-source-path (get-java-source-path)) )
    (block loop
      (dolist (dir java-source-path)
	(dolist (source-file-name source-file-names)
	  (let ( (full-source-file-name (concat dir source-file-name)) )
	    (if (file-exists-p full-source-file-name)
		(return-from loop full-source-file-name) ) ) ) ) ) ) )

;;-----------------------------------------------------------------
(setq java-package-regexp-list (list 
				(make-regexp '(seq (repeated (set " \t")) "package"
						   (repeated (set " \t")) (paren (repeated (set "A-Za-z0-9_.")))
						   (repeated (set " \t")) ";") )
				1))
									  
(setq java-import-regexp-list (list 
			       (make-regexp '(seq (repeated (set " \t")) "import"
						  (repeated (set " \t")) (paren (repeated (set "A-Za-z0-9_.*")))
						  (repeated (set " \t")) ";") )
			       1))

(setq java-class-start-regexp (make-regexp '(seq (repeated (set " \t")) "public"
						  (at-least-once (set " \t")) ) ) )

(defun java-packages-for-class (class-name)
  (block nil
    (let ( (packages '("java.lang")) )
      (save-excursion
	(goto-char (point-min))
	(while t
	  (let (package-name import-name)
	    (cond
	     ((setq package-name (first (match-regexp java-import-regexp-list)))
	      (cond
	       ((string-ends-with package-name (concat "." class-name))
		(return (list (substring package-name 0 (- (length package-name) (+ (length class-name) 1))))) )
	       ((string-ends-with package-name "*")
		(setq packages (cons (substring package-name 0 (- (length package-name) 2)) packages)) ) ) )
	     ((setq package-name (first (match-regexp java-package-regexp-list)))
	      (setq packages (cons package-name packages)) )
	     ((looking-at java-class-start-regexp)
	      (return packages)) ) )
	  (if (> (forward-line) 0) (return packages)) ) ) ) ) )

(defun show-java-packages-for-class ()
  (interactive)
  (let ( (class-name (word-at word-alpha-table (point))) )
    (if class-name
	(message "Packages are %S" (java-packages-for-class class-name))
      (message "No class name at point") ) ) )

(setq java-end-of-identifier-regexp (make-regexp '(at-least-once (not-set "A-Za-z0-9_$"))))

(defun java-type-for-variable (var-name)
  (if (or (not var-name) (equal var-name "this")) 
      (file-name-minus-extension (buffer-name))
    (save-excursion
      (let* ( (decl-regexp
	       (make-regexp `(seq (at-least-once (set "(,{ \t"))
				  (paren (seq (at-least-once (set "A-Za-z_"))
					      (repeated (set "A-Za-z0-9._"))) )
				  (paren (at-least-once (set "][ \t")))
				  ,var-name
				  (paren (repeated (set "][ \t")))
				  (at-least-once (set ";,=)"))
				  ) ) )
	      (decl-regexp-list (list decl-regexp 1 2 3)) declaration-found give-up type)
	(search-forward-regexp java-end-of-identifier-regexp)
	(while (not (or declaration-found give-up))
	  (if (search-backward-regexp decl-regexp nil t)
	      (let* ( (match-data (match-regexp decl-regexp-list))
		      (class (first match-data))
		      (brackets (concat (second match-data) (third match-data))) )
		(if (not (or (equal class "return") (equal class "instanceof")))
		    (progn 
		      (setq declaration-found t)
		      (setq type (strip-whitespace (concat class brackets))) ) ) )
	    (setq give-up t) ) )
	(if (not declaration-found)
	    (message "Type declaration for %s not found" var-name) )
	type) ) ) )

(defun java-get-array-base-type (type)
  (let ( (base-type type) )
    (while (string-ends-with base-type "[]")
      (setq base-type (substring base-type 0 (- (length base-type) 2))) )
    base-type) )

(defun show-java-type-for-variable ()
  (interactive)
  (let ( (var-name (word-at word-alpha-table (point))) )
    (if var-name
	(message "Type for %s is #%s#" var-name (java-type-for-variable var-name))
      (message "No var name at point") ) ) )

(defun java-identifier-at-point ()
  (word-at word-alpha-table (point)) )

(defun java-insert-import-line (class-name)
  (let* ( (import-headers-directory (project-directory :import-headers-directory))
	  (import-file (concat import-headers-directory class-name ".import"))
	  (wildcarded-file (concat import-headers-directory class-name ".wildcarded")) )
    (if (file-exists-p import-file)
	(let ( (include-file  (if (y-or-n-p "Wildcarded ? ") wildcarded-file import-file)) )
	  (if (file-exists-p include-file)
	      (save-excursion
		(goto-char (point-min))
		(search-forward-regexp "package")
		(end-of-line)
		(if (search-forward-regexp "import" nil t)
		    (beginning-of-line)
		  (insert "\n\n") )
		(insert-file include-file)
		(message "Import for %s inserted" class-name " from " include-file) )
	    (message "File %s does not exist" include-file) ) )
      (message "File %s does not exist" import-file) ) ) )

(defun java-insert-import-line-if-needed ()
  (interactive)
  (let ( (class-name (java-identifier-at-point)) )
    (if class-name
	(if (get-java-source-file-for-class class-name)
	    (message "Source file for %s can be found" class-name)
	  (java-insert-import-line class-name) )
      (message "No classname at point") ) ) )
  
(defun get-javadoc-url-for-class (class-name)
  (let ( packages )
    (and
     (setq base-type (java-get-array-base-type class-name))
     (setq packages (java-packages-for-class base-type))
     (find-javadoc-url packages base-type) ) ) )

(defun get-java-source-file-for-class (class-name)
  (let ( packages )
    (and
     (setq base-type (java-get-array-base-type class-name))
     (setq packages (java-packages-for-class base-type))
     (find-java-source-file packages base-type) ) ) )

(defun get-javadoc-url-for-variable (variable-name)
  (let ( class-name packages url )
    (and
     (setq class-name (java-type-for-variable variable-name))
     (get-javadoc-url-for-class class-name) ) ) )

(defun get-javadoc-url-for-identifier (identifier)
  (let ( url )
    (if (starts-upper-case identifier)
	(if (not (setq url (get-javadoc-url-for-class identifier)))
	    (setq url (get-javadoc-url-for-variable identifier)) )
      (if (not (setq url (get-javadoc-url-for-variable identifier)))
	  (setq url (get-javadoc-url-for-class identifier)) ) )
    url) )

(defvar javadoc-browser "iexplore.exe" "Browser to use to browse javadoc URL's")
(defvar javadoc-url-fragment "#method_summary" "Fragment to use when accessing javadoc URL's from java source code")

(defun browse-javadoc-url (url)
  (w32-shell-execute "open" javadoc-browser (concat url javadoc-url-fragment)) )

(defun show-javadoc-url ()
  (interactive)
  (let ( identifier url )
    (setq identifier (java-identifier-at-point))
    (if identifier
	(progn
	  (setq url (get-javadoc-url-for-identifier identifier))
	  (if url
	      (browse-javadoc-url url)
	    (message "No javadoc found for %s" identifier) ) )
      (message "No identifier at point") ) ) )

;;-----------------------------------------------------------------
(defvar java-extra-class-loading-args nil 
  "Extra args that might be needed to prevent static initializer errors loading Java classes")

(defun get-list-methods-startup-command ()
  (append
   (list *java-executable* "-cp" 
	(concat (get-java-classpath) ";" emacs-util-classpath) )
   (list "com1729.methods.JavaHelper") ) )

(defun get-list-methods-input-line (packages class-name method-start is-static)
  (concat (if is-static "list_static_members#" "list_instance_members#")
	  (separated-values
	   (list class-name method-start
		 (separated-values packages ",") )
	   ":") ) )

(setq java-var-and-method-regexp
      (make-regexp '(seq (at-least-once (not-set "A-Za-Z0-9_"))
			 (paren (at-least-once (set "A-Za-Z0-9_")))
			 "."
			 (paren (repeated (set "A-Za-z0-9_")) ) ) ))

(setq java-var-and-method-regexp-list
      (list java-var-and-method-regexp 1 2) )

(defun test() 
  (interactive)
  (message (format "result=#%S#" (get-var-and-method-before-point))))

(defun get-var-and-method-before-point ()
  (let ( (here (point)) method-name method-start var-start var-end var-name)
    (save-excursion
      (setq method-start (point))
      (backward-char)
      (while (looking-at "[A-Za-z0-9_]")
	(setq method-start (point))
	(backward-char) )
      (setq method-name (buffer-substring-no-properties method-start here))
      (if (looking-at "[.]")
	  (progn
	    (setq var-end (point))
	    (setq var-start (point))
	    (backward-char)
	    (while (looking-at "[A-Za-z0-9_]")
	      (setq var-start (point))
	      (backward-char) )
	    (setq var-name (buffer-substring-no-properties var-start var-end)) ) )
      (list var-name method-name) ) ) )

(defun java-list-methods-completion ()
  (interactive)
  (let* ( (var-and-method (get-var-and-method-before-point))
	  (variable-name (first var-and-method))
	  (method-name (second var-and-method)) 
	  is-static class-name packages list-methods-startup-command list-methods-input-line)
    (if (null variable-name) (setq variable-name "this"))
    (and
     (if (starts-upper-case variable-name)
	 (progn
	   (setq is-static t)
	   (setq class-name variable-name) )
       (setq class-name (java-type-for-variable variable-name)) )
     (setq packages (java-packages-for-class class-name))
     (setq list-methods-startup-command (get-list-methods-startup-command))
     (setq list-methods-input-line (get-list-methods-input-line packages class-name method-name is-static))
     (show-completion-buffer "*java-methods*" list-methods-startup-command 
			     list-methods-input-line 'java-complete-method-function) ) ) )

(setq java-method-call-regexp (make-regexp '(or "." start)))

(defun java-complete-method-function (string)
  (let ( (var-start (point)) )
    (save-excursion
      (backward-char)
      (while (looking-at "[A-Za-z0-9_]")
	(setq var-start (point))
	(backward-char) ) )
    (delete-region var-start (point)) )
  (insert string)
  (goto-first-parameter-on-line) )

(defun visit-java-source-file-for-class ()
  (interactive)
  (let ( class-name source-file-name )
    (setq class-name (java-identifier-at-point))
    (if class-name
	(progn
	  (setq source-file-name (get-java-source-file-for-class class-name))
	  (if source-file-name
	      (find-file source-file-name)
	    (message "No source file found for %s" class-name) ) )
      (message "No identifier at point") ) ) )

(defun java-get-base-src-dir ()
  (block loop
    (let ( (lower-case-directory (string-to-lower-case default-directory)) )
      (dolist (base-dir (get-java-source-path))
	(if (string-starts-with lower-case-directory (string-to-lower-case base-dir))
	    (return-from loop base-dir)) ) )
    (return-from loop nil)) )

(defun java-get-package ()
  (block loop
    (let ( (lower-case-directory (string-to-lower-case default-directory)) )
      (dolist (base-dir (get-java-source-path))
	(if (string-starts-with lower-case-directory (string-to-lower-case base-dir))
	    (let* ( (sub-path (substring default-directory (length base-dir))) package)
	      (if (string-starts-with sub-path "/")
		  (setq sub-path (substring sub-path 1)) )
	      (if (string-ends-with sub-path "/")
		  (setq sub-path (substring sub-path 0 (1- (length sub-path)))) )
	      (if (> (length sub-path) 0)
		  (return-from loop (replace-char sub-path ?/ ?.)) ) ) ) ) ) ) )

(defun java-get-full-class-name ()
  (let* ( (package (java-get-package))
	  (class-name (file-name-minus-extension (buffer-name))) )
    (if package
	(concat package "." class-name)
      class-name) ) )

;<target name="run" depends="classes">
;  <java fork="yes" classname="${run.class}" dir="${run.dir}" classpath="${run.classpath}" />
;</target>

(defvar java-run-main-set-run-dir nil "Whether java-run-main should include run.dir")

(defun java-run-main ()
  (interactive)
  (let* ( (class-name (java-get-full-class-name))
	  (compilation-command (concat "ant -emacs -find build.xml -Drun.class=" class-name 
				   (if java-run-main-set-run-dir
				       (concat " -Drun.dir=" default-directory )
				     "")
				   " run")) )
    (compile-with-command compilation-command) ) )

(defun java-run-file (file)
  (let ( (java-run-file-function (project-value :java-run-file-function)) )
    (if java-run-file-function
	(apply java-run-file-function nil)
      (java-run-main) ) ) )

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
  (setq run-file-function #'java-run-file)
  (setq filter-regexp java-filter-regexp)
  (setq c-basic-offset 2)
  (local-set-key [?\C-f] 'make-for-loop)
  (local-set-key [?\M-R] 'java-run-main)
  (local-set-key [?\C-t] 'insert-this-equals)
  (local-set-key [?\C-v] 'toggle-private)
  (local-set-key [?\C-w] 'java-search-for-identifier-at-point)
  (local-set-key "=" 'insert-spaced-equals)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key [?\C-a] 'insert-routine-args)
  (local-set-key [?\M-G] 'java-make-getter)
  (local-set-key [?\M-H] 'java-make-setter)
  (local-set-key [?\M-J] 'java-make-setter-and-getter)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [f2] 'my-expand-abbrev)
  (local-set-key [f10] 'shift-initial-case)
  (local-set-key [S-f10] 'java-make-lower-case-var)
  (setq word-alpha-table java-word-table)
  (setq for-loop-variable-declarer "int")
  (setq indent-tabs-mode nil)
  (local-set-key [C-f8] 'visit-java-source-file-for-class)
  (local-set-key [C-M-f8] 'java-insert-import-line-if-needed)
  (local-set-key [S-f8] 'show-javadoc-url)
  (local-set-key [f8] 'java-list-methods-completion)
  (font-lock-mode 1)
  (setq comment-start "/*")
  (setq comment-end "*/")
  (setq require-final-newline t) )

;;-----------------------------------------------------------------
(defun java-search-for-identifier-at-point ()
  (interactive)
  (let ( (word (word-at word-alpha-table (point))) )
    (if word
	(java-search-for-identifier word)
      (call-interactively 'java-search-for-identifier) ) ) )

(defun java-search-for-identifier (identifier)
  (interactive "sSearch for: ")
  (show-search-buffer (list (java-get-base-src-dir)) '(".java") identifier) )

(set-abbrev-language 'java)

(try-to-load "java-abbrev")

(defvar *java-main-source-file* "" "Current java main program")

(defun run-java-client ()
  (interactive)
  (save-this-buffer-and-others)
  (find-file *java-main-source-file*)
  (java-run-main)
  (delete-other-windows) )

(defun prefixed (prefix strings)
  (mapcar #'(lambda (string) (concat prefix string)) strings) )

(defun prefixed-paths (prefix paths) 
  (mapcar 
   #'(lambda (path)
       (cons (car path) (prefixed prefix (cdr path))) )
   paths) )

