
(add-hook 'idris-mode-hook 'my-idris-mode-hook)
(add-hook 'idris-hole-list-mode-hook 'my-idris-hole-list-mode-hook)
(add-hook 'idris-compiler-notes-mode-hook 'my-idris-compiler-notes-mode-hook)

(setq idris-word-table
      (make-alpha-table letters-digits-string "_'") )

(defun add-idris-defn()
  (interactive)
  (beginning-of-line)
  (search-forward-regexp "\\sw")
  (idris-add-clause nil) )

(defun insert-idris-function-arrow ()
  (interactive)
  (just-one-space)
  (insert "-> ") )

(defun insert-idris-equals ()
  (interactive)
  (just-one-space)
  (insert "= ") )

(defun insert-idris-constraint-arrow ()
  (interactive)
  (just-one-space)
  (insert "=> ") )

(defun wrap-as-idris-parameter()
  (interactive)
  (just-one-space 0)
  (insert " : ) -> ")
  (backward-char 8)
  (backward-sexp)
  (insert "(")
  (forward-sexp)
  (forward-char 3) )

(defun replace-identifier (old new)
  (interactive "sOld: \nsNew: ")
  (let ( (case-fold-search nil)
         (case-replace nil)
         (regex (concat "\\([^a-zA-Z_0-9']\\)" old "\\([^a-zA-Z_0-9']\\)"))
         (replacement (concat "\\1" new "\\2")) )
  (query-replace-regexp regex replacement) ) )

;; To avoid the set-line-filter function from triggering the hook to clear all Idris overlays ...
(eval-after-load "idris-highlight-input"
  '(progn
     (message "OVERRIDE idris-highlight--overlay-modification-hook to do nothing.")
     (defun idris-highlight--overlay-modification-hook (&rest args) ) ) )

(defun my-idris-mode-hook()
  (setq abbreviation-language 'idris)
  (setq word-alpha-table idris-word-table)
  (local-set-key [C-M-f9] 'idris-load-file)
  (local-set-key [M-f9] 'idris-compile-and-execute)
  (local-set-key [C-f8] 'idris-proof-search)
  (local-set-key [S-f8] 'add-idris-defn)  
  (local-set-key [f8] 'idris-case-dwim)
  (local-set-key [?\C-.] 'insert-idris-function-arrow)
  (local-set-key [?\C->] 'insert-idris-constraint-arrow)
  (local-set-key [?\C-=] 'insert-idris-equals)
  (local-set-key [?\C-:] 'wrap-as-idris-parameter)
  (local-set-key [M-up] 'idris-next-error)
  (unset-keys-hijacked-by-idris-mode) )

;;(setq idris-interpreter-flags '("--allow-capitalized-pattern-variables"))

(set-abbrev-language 'idris)

(defun my-idris-hole-list-mode-hook()
  (unset-keys-hijacked-by-idris-mode) )

(defun my-idris-compiler-notes-mode-hook ()
  (unset-keys-hijacked-by-idris-mode) )

(defun unset-keys-hijacked-by-idris-mode()
  (local-unset-key [delete])
  (local-unset-key "\M-n")
  (local-unset-key [mouse-3])
  )

(defun set-dual-frames()
  (interactive)
  (let ( (this-frame (selected-frame)) )
    (set-frame-parameter this-frame 'width 144)
    (set-frame-parameter this-frame 'height 53)
    (set-frame-parameter this-frame 'left 0)
    (set-frame-parameter this-frame 'top 0)
    (make-frame '((width . 144)
                  (height . 23)
                  (left . 450)
                  (top . 80) ) ) ) )

(set-abbrevs 
 'idris
 '( 
   ("pe" "public export")
   ("dt" "%default total\n\n")
   ("l" ("\n  let e1 = " mark "\n      \n  in " goto-mark))
   ("ll" ("let lemma = " mark " in" goto-mark))
   ("h" "?hole")
   ("th" ("the (" mark ") $ " goto-mark)) ) )
