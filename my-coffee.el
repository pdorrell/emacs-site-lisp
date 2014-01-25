(defun coffee-run-file (file &rest args)
  (let ( (run-coffee-function (project-value :run-coffee-function #'nodejs-run-file)) )
    (apply run-coffee-function (cons file args)) ) )

(defun coffee-hook ()
  "coffee-mode-hook"
  (setq expansion-key 'coffee-expansion-key)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key "=" 'insert-spaced-equals)
  (setq run-file-function #'coffee-run-file)
  (local-set-key [f2] 'my-expand-abbrev)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [?\C-\S-p] 'javascript-insert-print-this-inspected)
  (setq word-alpha-table javascript-word-table)

  )

(add-hook 'coffee-mode-hook '(lambda () (coffee-hook)))

(defun def-coffee-abbrev (abbrev expansion)
  "Define ABBREV to have EXPANSION in coffeescript mode"
  (set-abbrev abbrev expansion 'coffee-expansion-key) )

(def-coffee-abbrev "p" "console.log(\"")

