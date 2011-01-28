(add-hook 'python-mode-hook 'python-mode-hook-function)

(setq python-word-table
      (make-alpha-table letters-digits-string "_") )

(condition-case nil (kill-buffer "*python*") (error nil))

(defun python-run-file (file)
  (interactive)
  (let ( (filename (windowize-filename file)) )
    (setq *show-project-log-buffer-function* 'show-python-output)
    (condition-case nil (kill-buffer "*python*") (error nil))
    (switch-to-buffer-other-window "*python*")
    (setq *current-output-buffer* "*python*")
    (let ( (python-executable (project-file :python-executable)) )
      (message "%s %s ..." python-executable filename)
      (start-process "python" "*python*" python-executable  "-u" filename) ) ) )

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
  (setq expansion-key 'python-expansion-key)
  (setq run-file-function #'python-run-file)
  (local-set-key [C-M-f11] 'python-run-this-file-with-localenv)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [f2] 'my-expand-abbrev)
  (local-set-key "=" 'insert-spaced-equals)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key [f10] 'shift-initial-case)
  (local-set-key [?\C-t] 'insert-self-equals)
  (local-set-key [?\C-m] 'return-and-indent)
  (local-set-key [?\M-s] 'python-insert-self-dot)
  (setq word-alpha-table python-word-table)
  (font-lock-mode 1)
  (setq filter-regexp "def\\|class")
  (setq require-final-newline nil) )

(defun python-insert-self-dot()
  (interactive)
  (insert "self.") )

(defun def-python-abbrev (abbrev expansion)
  "Define ABBREV to have EXPANSION in python mode"
  (set-abbrev abbrev expansion 'python-expansion-key) )

(def-python-abbrev "di" '("def __init__(self):" indent return indent))
(def-python-abbrev "d" '("def " mark "(self):" goto-mark))
(def-python-abbrev "r" '("return "))
(def-python-abbrev "rf" '("return false"))
(def-python-abbrev "p" '("print(\""))
(def-python-abbrev "sae" '("self.assertEquals (" mark ")" goto-mark))
(def-python-abbrev "sane" '("self.assertNotEquals (" mark ")" goto-mark))
(def-python-abbrev "f" '("for " mark " in :" goto-mark))

(def-python-abbrev "q" '("\"\"\"" mark "\"\"\"" goto-mark))

(def-python-abbrev "main" '("if __name__ == \"__main__\":" return "main()" indent return))

(set-extension-mode ".egg" 'archive-mode)
