(require 'lean4-mode)

(defun lean4-mode-hook-function ()
  (setq programming-language 'lean4)
  )

(add-hook 'lean4-mode-hook 'lean4-mode-hook-function)

(set-abbrev-language 'lean4)

(set-abbrevs
 'lean4
 '(
   ("ch" "#check ")
   ("d" "def ")
   ("e" "#eval ") ) )
