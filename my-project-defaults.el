
(setq *project-type-default-values* (make-hash-table :test 'eq))

(defun set-project-type-default-values (project-type default-values)
  (declare (indent defun))
  (puthash project-type default-values *project-type-default-values*) )

(defun get-project-type-default-values (project-type)
  (gethash project-type *project-type-default-values*) )

(set-project-type-default-values 'emacs-site-lisp
  '( (:key value) ) )

(set-project-type-default-values 'general
  '( (:key value) ))

(set-project-type-default-values 'python
  '( ((:run-this-file . python) (other-window base-dir "run-python" this-file ("PY" . file)))
     (:build-function type-check-python)
     (:run-main-file (other-window base-dir "run-python" main-file 
                                   ("PYMAIN" . file) main-file-output-dir) ) ) )

(set-project-type-default-values 'python-uv
  '( ((:wrapper . python) "run-with-uv")
     (:build-function type-check-python)
     ((:run-this-file . python) (other-window base-dir (:wrapped python "python" "-u") this-file ("PY" . file)))
     (:run-main-file (other-window base-dir (:wrapped python "python" "-u") main-file ("PYMAIN" . file)
                                   main-file-output-dir)) ) )

(set-project-type-default-values 'python-with-venv
  '( ((:wrapper . python) "uv")
     (:build-function type-check-python)
     ((:run-this-file . python) (other-window base-dir (:wrapped python "python" "-u") this-file ("PY" . file)))
     (:run-main-file (other-window base-dir (:wrapped python "python" "-u") main-file ("PYMAIN" . file)
                                   main-file-output-dir)) ) )

(set-project-type-default-values 'regenerated-blog 
  '( (:alternate-file-or-dir-command (other-short-window-sync base-dir "run-regenerate" this-file-or-dir
                                                              ("REGEN" . file) )) ) )

(set-project-type-default-values 'sass
  '( (:sass-watch-command (other-window base-dir "run-sass-watch" nil ("SASS-WATCH" . nil) )) ) )

(set-project-type-default-values 'lean4
  '( (:key value) ) )


(set-project-type-default-values 'javascript
  '( (:key value)
     (:run-alternate-command (other-window base-dir "run-npm-start" nil nil nil webpack-line-matchers) )) )

(set-project-type-default-values 'typescript
  '( (:key value)
     (:build-function compile-typescript)
     (:run-project-command (other-window base-dir "run-npm-start" nil nil nil webpack-line-matchers) )
     (:alternate-file-or-dir-command (other-window base-dir "run-npm-test" this-file-or-dir ("NPM_TEST" . file)))
     (:run-alternate-command (other-window base-dir "run-npm-full-test" nil nil nil webpack-line-matchers) )) )

(set-project-type-default-values 'backup
  '( (:key value) ) )

(set-project-type-default-values 'vlog
  '( (:key value) ) )
  
