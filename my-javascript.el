;; Copyright (C) 2000-2013 Philip Dorrell

;;========================================================================
(setq javascript-word-table
      (make-alpha-table letters-digits-string "_$") )

(setq javascript-dotted-word-table
      (make-alpha-table letters-digits-string "._$") )

(set-extension-mode ".json" 'javascript-mode)

(set-extension-mode ".js" 'rjsx-mode)
(setq sgml-attribute-offset 0)

(eval-after-load 'js2-mode '(define-key js2-mode-map [down-mouse-3] nil))

(defun java-identifier-at-point ()
  (word-at word-alpha-table (point)) )

;;-----------------------------------------------------------------
(add-hook 'js-mode-hook 'javascript-hook)

(defun javascript-run-file (file &rest args)
  (let ( (run-javascript-function (project-value :run-javascript-function #'nodejs-run-file)) )
    (apply run-javascript-function (cons file args)) ) )

(defvar *nodejs-process* nil)

(defun nodejs-run-file (file &rest args)
  (let ( (filename (windowize-filename (expand-file-name file))) 
	 (current-directory default-directory) )
    (let ( (javascript-executable (project-file :javascript-executable "/usr/bin/nodejs")) )
      (switch-to-buffer-other-window "*nodejs*")
      (setq file-line-matchers
	    '((visit-grep-n-line unprefixed-grep-n-matcher)
	      (visit-grep-n-line node-exception-line-matcher)))
      (setq default-directory current-directory)
      (clear-buffer)
      (stop-then-start-process "nodejs" '*nodejs-process* "*nodejs*" 
			     javascript-executable (list filename) ) ) ) )


(defvar *phantomjs-process* nil)

(defun phantomjs-run-file (file &rest args)
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
  "Do puts \"x=#{x}\"; on preceding x"
  (interactive)
  (insert-tranformed-word 
   javascript-dotted-word-table 
   (lambda (var) (concat "console.log(\"" var " = \" + inspect(" var "));"))
   "dotted name") )

(defun javascript-hook ()
  (message "javascript-hook")
  (setq expansion-key 'javascript-expansion-key)
  (setq js-indent-level 2)
  (local-set-key [?\C-m] 'return-and-indent)
  (setq c-basic-offset 2)
  (local-set-key [?\C-f] 'make-for-loop)
  (local-set-key [?\C-t] 'insert-this-equals)
  (local-set-key "=" 'insert-spaced-equals)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [f2] 'my-expand-abbrev)
  (local-set-key [f10] 'shift-initial-case)
  (local-set-key [?\C-\S-p] 'javascript-insert-print-this-inspected)
  (setq word-alpha-table javascript-word-table)
  (setq for-loop-variable-declarer "var")
  (setq run-file-function #'javascript-run-file)
  (setq indent-tabs-mode nil)
  (font-lock-mode 1)
  (setq comment-start "/*")
  (setq comment-end "*/")
  (setq require-final-newline nil) )

;;-----------------------------------------------------------------
(defun def-javascript-abbrev (abbrev expansion)
  "Define ABBREV to have EXPANSION in javascript mode"
  (set-abbrev abbrev expansion 'javascript-expansion-key) )

(load "javascript-abbrev")
