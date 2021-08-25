;; Project values

(load-this-project
 `( (:key "value")
    (:project-name "hello_world")
    (:run-this-file ( (python . (other-window base-dir "run-python" this-file)) ))
    (:run-main-file (other-window base-dir "run-python" main-file))
    (:main-file "src/subdir/main.py")
    ) )
