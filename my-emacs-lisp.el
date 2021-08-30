;; Copyright (C) 2000,2001 Philip Dorrell

;;========================================================================
(setq emacs-lisp-mode-hook '(emacs-lisp-hook))

(setq emacs-lisp-filter-regexp
      (make-regexp '(at-least-once (set "("))) )

(setq emacs-lisp-word-alpha-table 
      (make-alpha-table letters-digits-string
			"_-@$%&*=+,<>?") )

(defun emacs-lisp-insert-message-this ()
  (interactive)
  (insert-tranformed-word 
   emacs-lisp-word-alpha-table 
  (lambda (var) (concat "(message \"" var " = %S\" " var ")"))
  "variable name") )

(defun emacs-lisp-hook()
  (setq programming-language 'lisp)
  (local-set-key [?\C-m] 'return-and-indent)
  (local-set-key [?\C-w] 'lisp-search-for-identifier-at-point)
  (local-set-key [?\C-\S-p] 'emacs-lisp-insert-message-this)
  (setq run-file-function #'load)
  (setq filter-regexp emacs-lisp-filter-regexp)
  (local-set-key [f2] 'my-expand-abbrev)
  (setq comment-start ";;") ; Single semi-colon not for comment on new line
  (setq comment-end "")
  (setq word-alpha-table emacs-lisp-word-alpha-table)
  (font-lock-mode 1)
  )

(defun lisp-search-for-identifier-at-point ()
  (interactive)
  (let ( (word (word-at word-alpha-table (point))) )
    (if word
	(lisp-search-for-identifier word)
      (call-interactively 'lisp-search-for-identifier) ) ) )

(defun lisp-search-for-identifier (identifier)
  (interactive "sSearch for: ")
  (show-search-buffer (list default-directory) '(".el" ".lisp") identifier) )

(defun buffer-for-name (name)
  (block nil
    (let ( (buffers (buffer-list)) )
      (dolist (buffer buffers)
	(if (equal (buffer-name buffer) name)
	    (return buffer) ) ) ) ) )

(defun messaged (var value)
  (message "%s=%s" var value)
  value)

(defun show-messages-buffer ()
  "Show the messages buffer"
  (interactive)
  (if (not (buffer-for-name "*Messages*")) (message ""))
  (display-buffer "*Messages*") )

;;-----------------------------------------------------------------
(set-abbrev-language 'lisp)

(set-abbrevs
 'lisp
 '(
   ("d" "(defun ")
   ("i" "(interactive)")
   ("l" "(local-set-key ")
   ("cldb" "cl-destructuring-bind ")
   ("copy" (concat ";; " copyright-line)) ) )

(defvar emacs-customisation-dir nil
  "File to visit in order to alter your own emacs customisations")

(defun edit-emacs-customisation()
  "Edit emacs-customisation"
  (interactive)
  (find-file (concat emacs-customisation-dir "/" file-menu-file-name)) )
