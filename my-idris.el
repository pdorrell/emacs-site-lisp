
(add-hook 'idris-mode-hook 'my-idris-mode-hook)

(defun my-idris-mode-hook()
  (local-set-key [mouse-3] 'paste-word)
  (local-set-key [S-mouse-3] 'prop-menu-show-menu)
  (local-set-key [C-M-f9] 'idris-load-file)
  )



