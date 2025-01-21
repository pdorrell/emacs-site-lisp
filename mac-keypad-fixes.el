(setq *all-keypad-fixes*
      '( ("kp-0" . "insert")
         ("kp-1" . "end")
         ("kp-2" . "down")
         ("kp-3" . "next")
         ("kp-4" . "left")
         ("kp-5" . "begin")
         ("kp-6" . "right")
         ("kp-7" . "home")
         ("kp-8" . "up")
         ("kp-9" . "prior")
         ("help" . "insert") ) )

(setq *help-keypad-fix*
      '( ("help" . "insert") ) )

(defun fix-keypad-key-mappings(keypad-fixes)
  (let ( (prefixes '("" "C-" "M-" "S-" "C-M-" "C-S-" "M-S-" "C-M-S-")) )
    (dolist (keypad-fix keypad-fixes)
      (let ( (in-value (car keypad-fix))
             (out-value (cdr keypad-fix)) )
        (dolist (prefix prefixes)
          (global-set-key (kbd (concat "<" prefix in-value ">")) 
                          (kbd (concat "<" prefix out-value ">"))) ) ) ) ) )

(fix-keypad-key-mappings *help-keypad-fix*)
