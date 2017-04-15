
(add-hook 'idris-mode-hook 'my-idris-mode-hook)

(defun add-idris-defn()
  (interactive)
  (beginning-of-line)
  (idris-add-clause nil) )

(defun my-idris-mode-hook()
  (local-set-key [mouse-3] 'paste-word)
  (local-set-key [S-mouse-3] 'prop-menu-show-menu)
  (local-set-key [C-M-f9] 'idris-load-file)
  (local-set-key [C-f8] 'idris-proof-search)
  (local-set-key [S-f8] 'add-idris-defn)  
  )

