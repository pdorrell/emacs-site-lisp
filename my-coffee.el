(defun coffee-hook ()
  "coffee-mode-hook"

  (local-set-key [?\C-=] 'insert-equals)
  )

(add-hook 'coffee-mode-hook '(lambda () (coffee-hook)))

