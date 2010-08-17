;; Copyright (C) 2000,2001 Philip Dorrell

;;========================================================================
(setq emacs-lisp-mode-hook '(emacs-lisp-hook))

(setq emacs-lisp-filter-regexp
      (make-regexp '(at-least-once (set "("))) )

(setq emacs-lisp-word-alpha-table 
      (make-alpha-table letters-digits-string
			"_-@$%&*=+,<>?") )

(defun emacs-lisp-hook()
  (setq expansion-key 'lisp-expansion-key)
  (local-set-key [?\C-m] 'return-and-indent)
  (local-set-key [?\C-w] 'lisp-search-for-identifier-at-point)
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
  (show-search-buffer (list default-directory) '(".el" ".plisp" ".lisp") identifier) )


(defun buffer-for-name (name)
  (block nil
    (let ( (buffers (buffer-list)) )
      (dolist (buffer buffers)
	(if (equal (buffer-name buffer) name)
	    (return buffer) ) ) ) ) )

(defun messaged (var value)
  (message "%s=%s" var value)
  value)

(buffer-for-name "*Messages*")

(defun show-messages-buffer ()
  "Show the messages buffer"
  (interactive)
  (if (not (buffer-for-name "*Messages*")) (message ""))
  (show-buffer nil "*Messages*") )

;;-----------------------------------------------------------------
(defun set-lisp-abbrev (abbrev expansion)
  "Define ABBREV to have EXPANSION in emacs-lisp mode"
  (set-abbrev abbrev expansion 'lisp-expansion-key) )

;;========================================================================
(set-lisp-abbrev "d" "(defun ")
(set-lisp-abbrev "i" "(interactive)")
(set-lisp-abbrev "g" "(global-set-key ")
(set-lisp-abbrev "l" "(local-set-key ")
(set-lisp-abbrev "copy" (concat ";; " copyright-line))

(global-set-key [?\M-z] 'show-messages-buffer)
