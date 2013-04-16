;; Copyright (C) 2000-2013 Philip Dorrell

;;========================================================================
(setq javascript-word-table
      (make-alpha-table letters-digits-string "_$") )

(defun java-identifier-at-point ()
  (word-at word-alpha-table (point)) )

;;-----------------------------------------------------------------
(add-hook 'js-mode-hook 'javascript-hook)

(defun javascript-hook ()
  (message "javascript-hook")
  (setq expansion-key 'javascript-expansion-key)
  (local-set-key [?\C-m] 'return-and-indent)
  (setq c-basic-offset 2)
  (local-set-key [?\C-f] 'make-for-loop)
  (local-set-key [?\C-t] 'insert-this-equals)
  (local-set-key "=" 'insert-spaced-equals)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [f2] 'my-expand-abbrev)
  (setq word-alpha-table javascript-word-table)
  (setq for-loop-variable-declarer "var")
  (setq indent-tabs-mode nil)
  (font-lock-mode 1)
  (setq comment-start "/*")
  (setq comment-end "*/")
  (setq require-final-newline nil) )

;;-----------------------------------------------------------------
(defun def-javascript-abbrev (abbrev expansion)
  "Define ABBREV to have EXPANSION in java mode"
  (set-abbrev abbrev expansion 'javascript-expansion-key) )

(load "javascript-abbrev")
