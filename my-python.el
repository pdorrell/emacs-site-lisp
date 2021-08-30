(add-hook 'python-mode-hook 'python-mode-hook-function)

(setq python-word-table
      (make-alpha-table letters-digits-string "_") )

(condition-case nil (kill-buffer "*python*") (error nil))

(defun execution-logging-time-string()
  (format-time-string "%Y-%m-%d %H:%M:%S") )

(defun python-run-file (file)
  (interactive)
  (let ( (filename (windowize-filename file)) )
    (setq *show-project-log-buffer-function* 'show-python-output)
    (condition-case nil (kill-buffer "*python*") (error nil))
    (switch-to-buffer-other-window "*python*")
    (setq *current-output-buffer* "*python*")
    (setq default-directory (project-base-directory))
    (let ( (python-executable (project-file :python-executable)) )
      (start-process "python" "*python*"
                     "/usr/bin/env" "PYTHONPATH=." python-executable "-u" filename) ) ) )

(defun python-run-nosetests (&optional qualifier)
  (interactive)
  (save-this-buffer-and-others)
  (let* ( (file-or-dir (buffer-full-file-or-directory-name))
          (nosetests-arg (if qualifier (concat file-or-dir ":" qualifier) file-or-dir)) )
    (condition-case nil (kill-buffer "*test-results*") (error nil))
    (switch-to-buffer-other-window "*test-results*")
    (let ( (nosetests-executable (project-file :nosetests-executable)) )
      (start-process "nosetests" "*test-results*" nosetests-executable nosetests-arg) ) ) )

(defun run-all-tests()
  (interactive)
  (save-this-buffer-and-others)
  (condition-case nil (kill-buffer "*test-results*") (error nil))
  (switch-to-buffer-other-window "*test-results*")
  (start-process "tests" "*test-results*" (concat (project-base-directory-value) "run-all-tests")) )

(defun python-run-nosetest-test-method()
  (interactive)
  (save-excursion
    (if (search-backward-regexp "^\\s-+def\\s-+\\([a-zA-z_0-9]+\\)" nil t)
        (let ( (method-name (match-string 1)) )
          (if (search-backward-regexp "^class\\s-+\\([a-zA-z_0-9]+\\)")
              (let ( (class-name (match-string 1)) )
                (python-run-nosetests (concat class-name "." method-name)) )
            (message "'class' definition not found") ) )
      (message "'def' method definition not found") ) ) )

(defun show-python-output()
  (interactive)
  (switch-to-buffer "*python*")
  (goto-char (point-max)) )

(defun python-run-main-file ()
  "Run main python file as defined by variable *python-main-file*"
  (interactive)
  (let ( (main-file (project-file :main-file)) )
    (message "Running main file %s ..." main-file)
    (python-run-file main-file) ) )

(defun python-run-this-file-with-localenv ()
  (interactive)
  (save-this-buffer-and-others)
  (let ( (filename (windowize-filename (expand-file-name (buffer-file-name)))) )
    (switch-to-buffer-other-window "*python*")
    (clear-buffer)
    (setq *current-output-buffer* "*python*")
    (start-process "python" "*python*" "env.exe" 
		   (concat "PYTHONPATH=" (getenv "PYTHONPATH") ";" (getenv "PYTHON_LOCALENV_PATH"))
		   (project-file :python-executable) "-u" filename) ) )

(defun insert-self-equals ()
  "Do self.x=x on preceding x"
  (interactive)
  (let* ( (var (word-before python-word-table (point)))
	  (member-var var)
	  article )
    (if var
	(progn
	  (delete-backward-char (length var))
	  (insert "self." member-var " = " var) )
      (message "No variable name given") ) ) )

(defun python-mode-hook-function ()
  (setq programming-language 'python)
  (setq run-file-function #'python-run-file)
  (local-set-key [C-M-f11] 'python-run-this-file-with-localenv)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [f2] 'my-expand-abbrev)
  (local-set-key "=" 'insert-spaced-equals)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key [f10] 'shift-initial-case)
  (local-set-key [C-S-f8] 'python-run-nosetest-test-method)
  (local-set-key [?\C-t] 'insert-self-equals)
  (local-set-key [?\C-m] 'return-and-indent)
  (local-set-key [?\M-s] 'python-insert-self-dot)
  (local-set-key [?\C-P] 'python-expand-to-print)
  (setq word-alpha-table python-word-table)
  (font-lock-mode 1)
  (setq filter-regexp "def\\|class")
  (setq require-final-newline nil) )

(defun python-insert-self-dot()
  (interactive)
  (insert "self.") )

(defun python-expand-to-print()
  (interactive)
  (beginning-of-line)
  (search-forward-regexp "\\S-.+")
  (let ( (expression (match-string 0)) )
    (replace-match (concat "print(\"" expression " = %r\" % " expression ")")) ) )

(set-abbrev-language 'python)

(set-abbrevs
 'python
 '(
   ("di" ("def __init__(self):" indent return indent))
   ("d" ("def " mark "(self):" goto-mark))
   ("r" ("return "))
   ("rf" ("return false"))
   ("p" ("print(\""))
   ("sae" ("self.assertEquals(" mark ")" goto-mark))
   ("sane" ("self.assertNotEquals(" mark ")" goto-mark))
   ("f" ("for " mark " in :" goto-mark))

   ("q" ("\"\"\"" mark "\"\"\"" goto-mark))

   ("main" ("if __name__ == \"__main__\":" return "main()" indent return)) ) )
