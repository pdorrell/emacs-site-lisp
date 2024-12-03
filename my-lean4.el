(require 'lean4-mode)

(defun lean4-mode-hook-function ()
  (setq programming-language 'lean4) )

(add-hook 'lean4-mode-hook 'lean4-mode-hook-function)

(define-key lean4-mode-map [f8] 'lean4-toggle-info)

(set-abbrev-language 'lean4)

(set-abbrevs
 'lean4
 '(
   ("as" "assumption")
   ("ch" "#check ")
   ("d" "def ")
   ("e" "#eval ") ) )


