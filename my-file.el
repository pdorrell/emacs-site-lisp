;; Copyright (C) 2000,2001 Philip Dorrell

;;========================================================================

(defvar file-menu-file-name "_" "Name used for file menus")

(defvar run-file-program "/usr/bin/gnome-open")

(defvar emacs-customisation-dir nil
  "File to visit in order to alter your own emacs customisations")

(defun edit-emacs-customisation()
  "Edit emacs-customisation"
  (interactive)
  (find-file (concat emacs-customisation-dir file-menu-file-name)) )

(defun kill-buffer-y-or-n ()
  "Kill current buffer"
  (interactive)
  (if (and (buffer-file-name) (buffer-modified-p))
      (progn
	(if (y-or-n-p (concat "Really lose changes to " (buffer-file-name) " ? "))
	  (progn (setq buffer-file-name nil) (kill-buffer nil)) )
	(message "") )
    (kill-buffer-safely) ) )
    

(defun save-and-kill-buffer ()
  "Save and then kill current buffer"
  (interactive)
  (if (and (buffer-file-name) (buffer-modified-p))
      (save-buffer) )
  (kill-buffer-safely) )

(defun kill-buffer-safely ()
  "Kill buffer if not a running shell (in which case try to exit)
and not unsaved file"
  (if (and running-windows-9x (eq major-mode 'shell-mode))
      (if (get-buffer-process (current-buffer))
	  (run-command-in-shell "exit")
	(kill-buffer nil) )
    (if (and (buffer-file-name) (buffer-modified-p))
	(message "Buffer has not been saved")
      (kill-buffer nil) ) ) )

(setq filename-word-table
      (make-alpha-table letters-digits-string
			"~@#$%^&-_=+\\/.*:") )

(setq non-whitespace-word-table
      (make-alpha-table letters-digits-string
			"`~!@#$%^&*()-_=+[{]};:'\"|\\,<.>/?") )

;;-----------------------------------------------------------------
;; Problem with mouse selection - functions cannot access it because the mouse selection
;; looks ahead at next key and resets selection before next key can get to it.
;; Kludgy hack requires use to type F5 to put mouse selection into place where functions
;; that use mouse selection if available can see it (and ignore non-visible marked region).

(defun mouse-selection-visible ()
  (and mark-active
       (eq (overlay-buffer mouse-drag-overlay) (current-buffer)) ) )

(defvar saved-mouse-selection nil "Mouse selection saved by save-mouse-selection")

(defun save-mouse-selection ()
  "Save mouse selection in saved-mouse-selection (because
currently mouse selector deletes selection before next key can be
processed."
  (interactive)
  (setq saved-mouse-selection 
	(if mark-active (buffer-substring (region-beginning) (region-end))) )
  (message saved-mouse-selection) )

(defun get-saved-mouse-selection ()
  "Get saved-mouse-selection and then set it to nil"
  (let ( (tmp saved-mouse-selection) )
    (setq saved-mouse-selection nil)
    tmp) )

;;-----------------------------------------------------------------

(setq quoted-filename-regexp (make-regexp '(seq (repeated (set " \t")) "\"" (paren (repeated (not-set "\""))) "\"")))

(defvar filename-at-point-function 'de-cygwined-filename-at-point "Function to get file name at point")

(make-variable-buffer-local 'filename-at-point-function)

(make-variable-buffer-local 'file-editor)

(defun edit-this-file ()
  "Edit this file in a (preferred) external editor"
  (interactive)
    (if (buffer-file-name)
	(progn
	  (message "Opening this file with %s ..." file-editor)
	  (call-process file-editor nil nil nil 
			(windowize-filename (expand-file-name (buffer-file-name))))
	  (message "File opened with %s" file-editor) )
      (message "No file to edit") ) )

(defun filename-at-point ()
  (if saved-mouse-selection
      (get-saved-mouse-selection)
    (if (eq major-mode 'dired-mode)
	(dired-get-filename)
      (if (looking-at quoted-filename-regexp)
	  (buffer-substring (match-beginning 1) (match-end 1))
	(funcall filename-at-point-function) ) ) ) )

(defun default-filename-at-point()
  (word-at filename-word-table (point)) )

(defun de-cygwined-filename-at-point()
  (let ( (filename (word-at filename-word-table (point))) )
    (if (string-starts-with filename "/cygdrive/")
	(concat (substring filename 10 11) ":" (substring filename 11))
      filename) ) )
    
(defun find-file-at-point ()
  "Find file whose name if any is at point"
  (interactive)
  (let ( (filename (filename-at-point)) )
    (if (or (not filename) (= (length filename) 0))
	(error "No file name at point") )
    (cond
     ((string-starts-with filename "****")
      (browse-url-in-buffer (substring filename 4)) )
     ((string-starts-with filename "***")
      (run-test-url (substring filename 3)) )
     ((string-starts-with filename "**")
      (run-file (substring filename 2)) )
     ((string-match "*" filename)
      (dired filename) )
     (t (messaged-find-file filename) ) ) ) )

(defvar run-test-url-function 'browse-url-in-buffer "Function to run on a test URL")

(defun run-test-url (url)
  "Run test URL"
  (funcall run-test-url-function url) )

(defun find-file-at-point-full-window ()
  "Find file whose name if any is at point and open full window"
  (interactive)
  (find-file-at-point)
  (delete-other-windows) )

(defun explore-directory (dir)
  (w32-shell-execute "explore" (windowize-filename (expand-file-name dir))) )

(defun run-file (filename)
  (if (eql (string-match "http:" filename) 0)
      (browse-url filename)
    (let ( (expanded-file-name (expand-file-name filename)))
      (message "Opening file %s" expanded-file-name)
      (if (fboundp 'w32-shell-execute)
	  (w32-shell-execute "open" (windowize-filename expanded-file-name))
	(let ((process-connection-type nil))
	  (start-process expanded-file-name nil
	   run-file-program expanded-file-name ) ) ) ) ) )

(defun notepad-this-file ()
  "Edit this file in notepad (useful for printing)"
  (interactive)
    (if (buffer-file-name)
	(if (fboundp 'w32-shell-execute)
	    (w32-shell-execute "open" "notepad.exe " (windowize-filename (expand-file-name (buffer-file-name))))
	  (message "No notepad in this OS") )
      (message "No file to edit") ) )

(defun edit-this-file-in-external-text-editor ()
  "Edit this file in a (preferred) external text editor (useful for printing)"
  (interactive)
    (if (buffer-file-name)
	(call-process *external-text-editor* nil nil nil 
		      (windowize-filename (expand-file-name (buffer-file-name))))
      (message "No file to edit") ) )

(defun windowize-filename (filename)
  "Convert forward slashes to back slashes if necessary"
  (if running-windows
      (let* ( (new-name (copy-seq filename))
	      (len (length new-name)) ch)
	(dotimes (i len)
	  (if (= (aref new-name i) ?/)
	      (aset new-name i ?\\) ) )
	new-name)
    filename) )

(defun execute-this-file ()
  (interactive)
  (let ( (file-name (buffer-file-name)) )
    (if file-name
	(progn 
	  (save-this-buffer-and-others )
	  (run-file file-name) )
      (message "This buffer is not visiting a file") ) ) )

(defun open-this-directory ()
  "Open current directory in Windows explorer"
  (interactive)
  (run-file default-directory) )

(defun explore-this-directory ()
  "Explore current directory in Windows explorer"
  (interactive)
  (explore-directory default-directory) )

(defun run-file-at-point ()
  "Run the file whose name is at point"
  (interactive)
  (run-file (filename-at-point)) )

(defun paste-filename (event)
  "paste word"
  (interactive "e")
  (let ( (word (word-clicked-on event filename-word-table)) )
    (if word
	(insert word)
      (message "No word at mouse position") ) ) )

;-----------------------------------------------------------------
(defun char-not-eoln (ch)
  (and ch (/= ch 10)) )

(defun line-start (pos)
  "Start of line that pos is in"
  (let ( (start (1- pos)) )
    (while (char-not-eoln (char-after start))
      (setq start (1- start)) )
    (setq start (1+ start)) 
    start) )

(defun line-end (pos)
  "End of line that pos is in"
  (let ( (end pos) )
    (while (char-not-eoln (char-after end))
      (setq end (1+ end)) )
    end) )

(defun buffer-line (pos)
  (buffer-substring-no-properties 
   (line-start pos) (line-end pos) ) )

(setq python-line-matcher 
      (list "[ ]*File[ ]*\"\\([^\"]*\\)\",[ ]*line[ ]*\\([0-9]+\\)"
	    1 2) )

(defun visit-grep-n-line (file-name line-number-string)
  (let ( (line-number (string-to-number line-number-string)) )
    (find-file file-name)
    (goto-line line-number) ) )

;; [ \\t\\n]*\\(from \\)?
(setq grep-n-matcher (list "[ \t]*\\(?:from \\|\\)\\([a-zA-Z]?[:]?[^:]+\\):\\([0-9]+\\):?"
			   1 2) )

(setq java-exception-line-matcher
      (list (make-regexp '(seq (maybe "+") (at-least-once (set " \t")) "at " 
			       (paren (at-least-once (set "a-zA-Z0-9._$<>")))
			       "(" (paren (at-least-once (set "a-zA-Z0-9_"))) ".java:"
			       (paren (at-least-once (set "0-9"))) ")"))
	    1 2 3) )

(defun visit-java-exception-line (method-name class-name line-number-string)
  (let* ( (line-number (string-to-number line-number-string)) 
	  (package-name (package-from-method-name method-name))
	  (source-file (find-java-source-file (list package-name) class-name)) )
    (if source-file
	(progn
	  (find-file source-file)
	  (goto-line line-number) )
      (message "Cannot find java file %s.java in package %s" class-name package-name) ) ) )
	  

(defun package-from-method-name (method-name)
  (let ( (package-end (nth-last-pos method-name ?. 2)) )
    (if package-end
	(substring method-name 0 package-end)
      "") ) )

(defvar file-line-matchers
  '((visit-grep-n-line grep-n-matcher)
    (visit-grep-n-line python-line-matcher)
    (visit-java-exception-line java-exception-line-matcher)) )

(defun visit-file-line ()
  "Visit line of file given in grep output"
  (interactive)
  (let ( (fun-and-args
	  (block nil
	    (save-excursion
	      (beginning-of-line)
	      (dolist (file-line-matcher file-line-matchers)
		(let* ( (matcher (symbol-value (second file-line-matcher)))
			(this-args (match-regexp matcher)) )
		  (if this-args
		      (return (cons (first file-line-matcher) this-args)) ) ) ) ) ) ) )
    (if fun-and-args
	(let ( (fun (car fun-and-args))
	       (args (cdr fun-and-args)) )
	  (apply fun args) ) ) ) )

;-----------------------------------------------------------------

(defvar *base-file-menu*
  "~/_" "The base file menu")

(defun messaged-find-file (filename)
  (find-file filename)
  (message "%s" (buffer-file-name)) )

(defun open-base-file-menu ()
  "Open base file menu"
  (interactive)
  (messaged-find-file *base-file-menu*) )

(defun open-file-menu ()
  "Open file menu"
  (interactive)
  (messaged-find-file file-menu-file-name) )

(defun open-file-menu-other-window ()
  "Open file file menu in other window"
  (interactive)
  (find-file-other-window file-menu-file-name) 
  (message "%s" (buffer-file-name)) )

(defun open-base-file-menu-other-window ()
  "Open file file-menu in other window"
  (interactive)
  (find-file-other-window *base-file-menu*) 
  (message "%s" (buffer-file-name)) )

(defun save-this-buffer-and-others ()
  "Save visited file and any others with prompting"
  (interactive)
  (if (buffer-file-name) (save-buffer))
  (save-some-buffers) ) 

(defun dired-default-directory ()
  "Dired current directory"
  (interactive)
  (dired default-directory) )

;;-----------------------------------------------------------------

(defun dired-hook ()
  (local-unset-key [?\M-\C-d])
  (local-unset-key [?\M-\C-N])
  (local-set-key "r" 'wdired-change-to-wdired-mode)
  (font-lock-mode 1) )

(add-hook 'dired-mode-hook 'dired-hook)

(defun insert-old-file-menu ()
  (interactive)
  (insert-file-contents "file-menu") )

(defun delete-file-if-exists (file) 
  (if (file-exists-p file)
      (delete-file file) ) )

(global-set-key [?\C-\M-i] 'insert-old-file-menu)

;-----------------------------------------------------------------
(global-set-key [M-mouse-3] 'paste-filename)

(global-set-key [f3] 'save-buffer)
(global-set-key [S-f3] 'save-and-kill-buffer)
(global-set-key [M-f3] 'save-this-buffer-and-others)

(global-set-key [f4] 'find-file-at-point)
(global-set-key [C-f4] 'find-file-at-point-full-window)
(global-set-key [S-f4] 'visit-file-line)
(global-set-key [M-f4] 'run-file-at-point)
(global-set-key [S-M-f4] 'execute-this-file)

(global-set-key [f5] 'save-mouse-selection)

(global-set-key [f7] 'open-base-file-menu)
(global-set-key [M-f7] 'open-file-menu)
(global-set-key [S-f7] 'open-file-menu-other-window)
(global-set-key [S-M-f7] 'open-base-file-menu-other-window)
(global-set-key [C-f7] 'edit-emacs-customisation)

(global-set-key [?\M-\C-d] 'open-this-directory)
(global-set-key [?\M-\C-e] 'explore-this-directory)
(global-set-key [?\C-N] 'edit-this-file-in-external-text-editor)

(global-set-key [?\M-D] 'dired-default-directory)
(global-set-key [?\M-K] 'kill-buffer-y-or-n)
(global-set-key "\C-xK" 'kill-buffer-y-or-n)

