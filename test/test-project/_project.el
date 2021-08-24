;; Project values

(load-this-project
 `( (:key "value")
    (:name "hello_world")
    (:run-this-file ( (python . ( script-to-other-window project-base-directory-value "run-python" buffer-file-name)) ))
    ) )
