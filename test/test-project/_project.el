;; Project values

(load-this-project
 `( (:key "value")
    (:project-name "hello_world")
    (:run-this-file ( (python . (script-to-other-window project-base-directory-value "run-python" buffer-file-name)) ))
    (:run-main-file (script-to-other-window project-base-directory-value "run-python" get-project-main-file))
    (:main-file "src/subdir/main.py")
    ) )
