
(setq *project-type-default-values* (make-hash-table :test 'eq))

(defun set-project-type-default-values (project-type default-values)
  (declare (indent defun))
  (puthash project-type default-values *project-type-default-values*) )

(defun get-project-type-default-values (project-type)
  (gethash project-type *project-type-default-values*) )

(set-project-type-default-values 'python
  '( ((:run-this-file . python) (other-window base-dir "run-python" this-file))
     (:run-main-file (other-window base-dir "run-python" main-file)) ) )

(set-project-type-default-values 'python-with-venv
  '( ((:run-this-file . python) (other-window base-dir "run-python-in-venv" this-file))
     (:run-main-file (other-window base-dir "run-python-in-venv" main-file)) ) )

(set-project-type-default-values 'regenerated-blog 
  '( (:search-extensions (".rb" ".html" ".rhtml" ".css"))
     (:alternate-file-or-dir-command (other-short-window-sync base-dir "run-regenerate" this-file-or-dir))
     (:sass-watch-command (other-window base-dir "run-sass-watch" nil)) ) )
