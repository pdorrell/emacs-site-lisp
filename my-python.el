(add-hook 'python-mode-hook 'python-mode-hook-function)

(setq python-word-table
      (make-alpha-table letters-digits-string "_") )

(condition-case nil (kill-buffer "*python*") (error nil))

(defun python-search-for-identifier-at-point ()
  (interactive)
  (let ( (word (word-at word-alpha-table (point))) )
    (if word
	(python-search-for-identifier word)
      (call-interactively 'python-search-for-identifier) ) ) )

(defun python-search-for-identifier (identifier)
  (interactive "sSearch for: ")
  (show-search-buffer (list default-directory) '(".py") identifier) )


(defun python-run-file (file)
  (interactive)
  (save-this-buffer-and-others)
  (let ( (filename (windowize-filename file)) )
    (condition-case nil (kill-buffer "*python*") (error nil))
    (switch-to-buffer-other-window "*python*")
    (setq *current-output-buffer* "*python*")
    (start-process "python" "*python*" (project-file :python-executable) "-u" filename) ) )

(defun python-run-this-file ()
  (interactive)
  (python-run-file (expand-file-name (buffer-file-name))) )

(defun python-run-main-file ()
  "Run main python file as defined by variable *python-main-file*"
  (interactive)
  (python-run-file (project-file :main-file)) )

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
  (local-set-key [S-M-f9] 'python-run-main-file)
  (local-set-key [C-M-f11] 'python-run-this-file-with-localenv)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [f2] 'my-expand-abbrev)
  (local-set-key "=" 'insert-spaced-equals)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key [f10] 'shift-initial-case)
  (local-set-key [?\C-t] 'insert-self-equals)
  (local-set-key [?\C-m] 'return-and-indent)
  (local-set-key [?\M-s] 'python-insert-self-dot)
  (local-set-key [?\C-w] 'python-search-for-identifier-at-point)
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

(set-extension-mode ".egg" 'archive-mode)
