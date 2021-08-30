
(setq *project-type-default-values*
      '( 
        (python 
         ((:run-this-file . python) (other-window base-dir "run-python" this-file))
         (:run-main-file (other-window base-dir "run-python" main-file)) )
         
        (python-with-venv
         ((:run-this-file . python) (other-window base-dir "run-python-in-venv" this-file))
         (:run-main-file (other-window base-dir "run-python-in-venv" main-file)) )
         
        (regenerated-blog 
         (:search-extensions (".rb" ".html" ".rhtml" ".css"))
         (:alternate-file-or-dir-command (other-short-window-sync base-dir "run-regenerate" this-file-or-dir))
         (:sass-watch-command (other-window base-dir "run-sass-watch" nil)) )
        
        ) )
