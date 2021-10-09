(add-hook 'python-mode-hook 'python-mode-hook-function)

(setq python-word-table
      (make-alpha-table letters-digits-string "_") )

(condition-case nil (kill-buffer "*python*") (error nil))

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

(defconst python-default-line-filtering-regex 
  (make-regex '(seq start (group (one-of "def" "class")))))

(defun python-mode-hook-function ()
  (setq programming-language 'python)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [f2] 'my-expand-abbrev)
  (local-set-key "=" 'insert-spaced-equals)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key [f10] 'shift-initial-case)
  (local-set-key [C-S-f8] 'python-run-nosetest-test-method)
  (local-set-key [?\C-t] 'insert-self-equals)
  (local-set-key [?\C-m] 'return-and-indent)
  (local-set-key [?\M-s] 'python-insert-self-dot)
  (local-set-key [?\C-P] 'python-expand-to-print)
  (setq word-alpha-table python-word-table)
  (font-lock-mode 1)
  (setq default-line-filtering-regex python-default-line-filtering-regex)
  (setq *trim-trailing-whitespace-on-save* t)
  (setq require-final-newline t) )

(defun python-insert-self-dot()
  (interactive)
  (insert "self.") )

(defun python-expand-to-print()
  (interactive)
  (beginning-of-line)
  (search-forward-regexp "\\S-.*")
  (let ( (expression (match-string 0)) )
    (replace-match (concat "print(\"" expression " = %r\" % (" expression ",))")) ) )

(let ( (non-identifier-char-regex "[^A-Za-z0-9_]") )
  (set-language-search-regexes 'python
    :before-identifier (format "(^|%s)" non-identifier-char-regex)
    :after-identifier (format "($|%s)" non-identifier-char-regex)
    :before-definition "(def|class)\s+"
    :after-definition (format "($|%s)" non-identifier-char-regex) ) )

(set-abbrev-language 'python)

(set-abbrevs
 'python
 '(
   ("di" ("def __init__(self):" indent return indent))
   ("d" ("def " mark "(self):" goto-mark))
   ("r" ("return "))
   ("cm" ("@classmethod"))
   ("rf" ("return false"))
   ("p" ("print(\""))
   ("sae" ("self.assertEquals(" mark ")" goto-mark))
   ("sane" ("self.assertNotEquals(" mark ")" goto-mark))
   ("f" ("for " mark " in :" goto-mark))

   ("q" ("\"\"\"" mark "\"\"\"" goto-mark))

   ("main" ("if __name__ == \"__main__\":" return "main()" indent return)) ) )
