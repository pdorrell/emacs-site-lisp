;; Copyright (C) 2000,2001 Philip Dorrell

;;========================================================================

(defvar rake-command "rake")
(defvar rake-file "rakefile")
(defvar *current-output-buffer* nil "Last buffer which for the output of running or compiling something")

(defun find-parent-directory-with-file (dir filename)
  (let ( (parent dir) (found nil) (finished nil))
    (while (and (not found) (not finished))
      (if (file-exists-p (expand-file-name filename parent))
	  (setq found t)
	(let ( (newparent (file-name-directory (substring parent 0 -1))) )
	  (setq finished (equal newparent parent))
	  (setq parent newparent) ) ) )
    (and found parent)) )

(defun compile-with-command (command &optional command-file base-dir-key)
  (let ( (compilation-base-dir nil) )
    (if base-dir-key 
        (setq compilation-base-dir (project-file base-dir-key)) )
    (if (not compilation-base-dir)
        (setq compilation-base-dir (get-current-project-base-directory)) )
    (save-this-buffer-and-others)
    (if command-file
	(let ( (command-file-dir (find-parent-directory-with-file default-directory command-file)) )
	  (if command-file-dir
	      (find-file (concat command-file-dir command-file)) ) ) )
    (compile command)
    (other-window 1)
    (switch-to-buffer "*compilation*")
    (if compilation-base-dir
	(progn
	  (message "In compile-with-command, cd compilation buffer to %s" compilation-base-dir)
	  (cd compilation-base-dir) ) )
    (setq *current-output-buffer* "*compilation*")
    (end-of-buffer) ) )

(defun compile-rake (&optional target)
  "Compile using rake"
  (compile-with-command (if target (concat rake-command " " target) rake-command)) )

(defun compile-ant ()
  "Compile using ant"
  (interactive)
  (compile-with-command "ant -emacs -find build.xml") )

(defvar ant-before-target "" "extra stuff to put before ant target")
(defvar ant-after-target "" "extra stuff to put after ant target")

(defun compile-ant-target (target)
  "Compile using ant"
  (interactive "starget: ")
  (compile-with-command (concat "ant -emacs -find build.xml " target)) )

(defun previous-error ()
  "Go to previous error"
  (interactive)
  (next-error -1) )

(defun visit-output-buffer()
  (interactive)
  (if *current-output-buffer*
      (show-buffer nil *current-output-buffer*)
    (message "No current output buffer to visit") ) )

(defun git-gui-this-directory()
  "Start git gui in this directory"
  (interactive)
  (message "starting git-gui-this-directory ...")
  (set-process-query-on-exit-flag (start-process "gitgui" nil *git-executable* "gui") nil) )

(defun gitk-this-directory()
  "Start gitk in this directory"
  (interactive)
  (message "starting gitk-this-directory ...")
  (set-process-query-on-exit-flag (start-process "gitk" nil *gitk-executable* "--all") nil) )

(defun git-fetch-and-view()
  (interactive)
  (let ( (git-fetch-and-view-script (expand-file-name "bin/git-fetch-and-view" emacs-customisation-dir)) )
    (script-to-other-window git-fetch-and-view-script (get-current-project-base-directory)
                            "git-fetch-and-view" '()) ) )

(defun run-alternate-command()
  "Compile this file"
  (interactive)
  (eval (project-required-value :alternate-run-command)) )

(defun insert-tranformed-word (word-table transformer description)
  "Replace preceding word in buffer (according to WORD-TABLE) by applying TRANSFORMER to it.
  If no word is found to replace, warn that the DESCRIPTION is not there."
  (let* ( (word (word-before word-table (point))) )
    (if word
	(progn
	  (delete-backward-char (length word))
	  (insert (funcall transformer word)) )
      (message "No %s given" descriptipon) ) ) )

(define-minor-mode stop-on-1st-test-failure-minor-mode
  "When running unit tests, stop on first failure"
  nil " Stop1stFail" :global t)

;;====================================================================================================
;; Shared programming editing functions (shared between different languages)

(defun insert-this-equals ()
  "Do this.x=x on preceding x (used by javascript mode)"
  (interactive)
  (let* ( (var (word-before word-alpha-table (point)))
	  (member-var var) )
    (if var
	(progn
	  (delete-backward-char (length var))
	  (insert "this." member-var " = " var ";") )
      (message "No variable name given") ) ) )

(setq equals-op-table (make-alpha-table "+-=/*&|^%<>!"))

(defun insert-spaced-equals ()
  "Insert = with spaced around, unless part of another operator"
  (interactive)
  (let* ( (point (point))
	  (prev-char (char-after (- point 1)))
	  (prev-prev-char (char-after (- point 2))) )
    (if (and prev-char (aref equals-op-table prev-char))
	(progn
	  (forward-char -1)
	  (insert-space-if-not-there)
	  (forward-char 1)
	  (insert "= ") )
      (if (and (= prev-prev-char 61) (= prev-char 32))
	  (progn (delete-backward-char 1) (insert "= "))
	(progn (insert-space-if-not-there) (insert "= ") ) ) ) ) )

(defun insert-equals ()
  "Insert '-', without doing anything else fancy."
  (interactive)
  (insert "=") )

(defun insert-spaced-comma ()
  "Insert ',' with space after"
  (interactive)
  (if (looking-at " ")
      (insert ",")
    (insert ", ") ) )

(defun return-and-indent ()
  "Insert new line and indent."
  (interactive)
  (newline)
  (indent-for-tab-command) )

(defun start-single-quoted-string()
  "Insert two single quotes with point inside them."
  (interactive)
  (insert "''")
  (backward-char 1) )

(defun start-double-quoted-string()
  "Insert two double quotes with point inside them."
  (interactive)
  (insert "\"\"")
  (backward-char 1) )

(defun insert-space-if-not-there()
  "Insert a space character if it isn't already there"
  (let* ( (point (point))
          (prev-char (char-after (- point 1))) )
    (if (not (= prev-char 32))
        (insert " ")) ) )

(make-variable-buffer-local 'for-loop-variable-declarer)

(defun make-for-loop ()
  "Make for loop using preceding variable name (used by javascript-mode)"
  (interactive)
  (let ( (var (word-before word-alpha-table (point)))
	 saved-point)
    (if var
	(progn
	  (delete-backward-char (length var))
	  (insert "for (" for-loop-variable-declarer " " var "=0; " var "<")
	  (setq saved-point (point))
	  (insert "; " var "++) {\n")
	  (indent-for-tab-command)
	  (insert "\n}") (indent-for-tab-command)
	  (goto-char saved-point) )
      (message "No variable name given for for loop") ) ) )
