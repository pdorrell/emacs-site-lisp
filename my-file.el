;; Copyright (C) 2000,2001 Philip Dorrell

;;========================================================================

(defvar run-file-program "/usr/bin/xdg-open")

(defun kill-buffer-y-or-n ()
  "Kill current buffer, after confirming loss of any unsaved changes."
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
  (if (and (buffer-file-name) (buffer-modified-p))
      (message "Buffer has not been saved")
    (kill-buffer nil) ) )

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
  "Is the mouse selection currently visible?"
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
  (message "saved-mouse-selection = %S" saved-mouse-selection) )

(defun get-saved-mouse-selection ()
  "Get saved-mouse-selection and then set it to nil"
  (let ( (tmp saved-mouse-selection) )
    (setq saved-mouse-selection nil)
    tmp) )

;;-----------------------------------------------------------------

(defvar filename-at-point-function 'default-filename-at-point "Function to get file name at point")

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
  "Get filename at point, 1st look for saved-mouse-selection, 2nd if in dired, get the filename,
   3rd call the filename-at-point-function for the buffer."
  (if saved-mouse-selection
      (get-saved-mouse-selection)
    (if (eq major-mode 'dired-mode)
	(dired-get-filename)
      (funcall filename-at-point-function) ) ) )

(defun default-filename-at-point()
  "Default filename-at-point-function value, get a 'word' using filename-word-table"
  (word-at filename-word-table (point)) )

(defun find-file-at-point ()
  "Find file whose name if any is at point"
  (interactive)
  (let ( (filename (filename-at-point)) )
    (if (or (not filename) (= (length filename) 0))
	(message "No file name at point")
      (messaged-find-file filename) ) ) )

(defvar run-test-url-function 'browse-url-in-buffer "Function to run on a test URL")

(defun run-test-url (url)
  "Run test URL"
  (funcall run-test-url-function url) )

(defun find-file-at-point-full-window ()
  "Find file whose name if any is at point and open full window"
  (interactive)
  (find-file-at-point)
  (delete-other-windows) )

(defun browse-in-dev-browser(url)
  (start-process "chromium-dev" nil "chromium-browser" "--allow-file-access-from-files" "--disable-popup-blocking" url) )

(defun run-file (filename)
  "Run file using OS 'run file' - treat URL's as a special case."
  (if (or (string-ends-with filename ".html")
	  (eql (string-match "http\\(s\\|\\):" filename) 0) )
      (browse-in-dev-browser filename)
    (let ( (expanded-file-name (expand-file-name filename)))
      (message "Opening file %s" expanded-file-name)
      (let ((process-connection-type nil))
	(start-process expanded-file-name nil
	               run-file-program expanded-file-name ) ) ) ) )

(defun edit-this-file-in-external-text-editor ()
  "Edit this file in a (preferred) external text editor (useful for printing)"
  (interactive)
    (if (buffer-file-name)
	(call-process *external-text-editor* nil nil nil 
		      (windowize-filename (expand-file-name (buffer-file-name))))
      (message "No file to edit") ) )

(defun windowize-filename (filename)
  "Do nothing"
  filename)

(defun execute-this-file ()
  "Run the file being edited."
  (interactive)
  (let ( (file-name (buffer-file-name)) )
    (if file-name
	(progn 
	  (save-this-buffer-and-others )
	  (run-file file-name) )
      (message "This buffer is not visiting a file") ) ) )

(defun open-this-directory ()
  "Open current directory in OS file manager"
  (interactive)
  (run-file default-directory) )

(defun run-file-at-point ()
  "Run the file whose name is at point"
  (interactive)
  (run-file (filename-at-point)) )

(defun paste-filename (event)
  "paste word that is a filename"
  (interactive "e")
  (let ( (word (word-clicked-on event filename-word-table)) )
    (if word
	(insert word)
      (message "No word at mouse position") ) ) )

;;-----------------------------------------------------------------
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
  "Unset various local keys that mask preferred global keys"
  (local-unset-key [?\M-\C-d])
  (local-unset-key [?\M-\C-N])
  (local-set-key "r" 'wdired-change-to-wdired-mode)
  (font-lock-mode 1) )

(add-hook 'dired-mode-hook 'dired-hook)
