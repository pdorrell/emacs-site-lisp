;; Project values

(load-this-project
 `( (:project-name "hello_world")
    (:project-type python)
    (:alternate-command-dir "src/subdir_with_files")
    (:run-alternate-command (other-window-show-top alternate-command-dir "list-files" nil ("LIST_FILES" . nil)))
    (:main-file "src/subdir/main.py")
    ) )
