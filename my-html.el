;; Copyright (C) 2000 Philip Dorrell

(require 'web-mode)

(dolist (extension '(".erb" ".html"))
  (set-extension-mode extension 'web-mode) )

(setq web-mode-markup-indent-offset 2)

(defmacro def-html-pair-abbrev (abbrev part1 &optional part2)
  (if part2
      `(def-html-abbrev ,abbrev '(,part1 mark ,part2 goto-mark))
    `(def-html-abbrev ,abbrev ,part1) ) )  

(defun def-html-abbrev (abbrev expansion)
  (put (intern abbrev) 'html-expansion expansion) )

(defun get-html-abbrev-before()
   (let ((start (1- (point))) (end (point)) )
    (while (buffer-char-in-table abbrev-word-table start)
      (setq start (1- start)) )
    (setq start (1+ start))
    (if (> end start)
	(buffer-substring-no-properties start end)
      nil) ) )

(defun html-wrap-selection-with-tags (before-tag after-tag)
  (let ( (start (region-beginning))
	 (end (region-end)) )
    (if (and start end)
	(progn
	  (goto-char start)
	  (insert before-tag)
	  (goto-char end) (forward-char (length before-tag))
	  (insert after-tag)) ) ) )

(defun html-make-bold ()
  (interactive)
  (html-wrap-selection-with-tags "<b>" "</b>") )

(defun html-make-italic ()
  (interactive)
  (html-wrap-selection-with-tags "<i>" "</i>") )

(defun html-make-ahref ()
  (interactive)
  (html-wrap-selection-with-tags "<a href=\"\">" "</a>") )

(defun expand-html-abbrev ()
  "Expand abbreviation defined using set-html-abbrev"
  (interactive)
  (let ( (abbrev (get-html-abbrev-before)) expansion)
    (if abbrev
	(progn
	  (setq expansion (get (intern abbrev) 'html-expansion))
	  (if (not expansion)
	      (setq expansion 
		    (list (concat "<" abbrev ">") 'mark (concat "</" abbrev ">") 'goto-mark) ) )
	  (delete-backward-char (length abbrev))
	  (if (listp expansion)
	      (complex-expand expansion)
	    (insert expansion) ) ) ) ) )

(defun make-wiki-link ()
  "Convert word before cursor to a wiki-style link"
  (interactive)
  (let ( (name (word-before word-alpha-table (point))) )
    (if name
	(progn
	  (delete-backward-char (length name))
	  (insert "<a href=\"" name ".html\">" name "</a>") )
      (message "No word before cursor") ) ) )

(defvar html-mode-keymap (make-sparse-keymap))

(defvar html-abbrev-word-table 
  (make-alpha-table "@#abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"))

(defvar html-alpha-table
  (make-alpha-table "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_") )

(setq xml-filter-regexp
      (make-regexp '(seq (repeated (set " \t")) "<" (at-least-once (not-set "/>")) ">")) )

(defun web-mode-hook-function ()
  (setq indent-tabs-mode nil)
  (use-local-map html-mode-keymap)
  (setq abbrev-word-table html-abbrev-word-table)
  (setf word-alpha-table html-alpha-table)
  (setq filter-regexp xml-filter-regexp)
  (local-set-key [f10] 'shift-initial-case)
  (local-set-key [?\M-\C-\ ] "&nbsp;")
  (local-set-key [f2] 'expand-html-abbrev)
  (setq run-file-function #'open-file-in-web-browser)
  (local-set-key [?\M-e] 'edit-this-file)
  (local-set-key [?\C-b] 'html-make-bold)
  (local-set-key [?\M-\C-i] 'html-make-italic)
  (local-set-key [?\C-a] 'html-make-ahref)
  (font-lock-mode 1)
  (setq file-editor *external-html-editor*)
  )

(defun regenerate-this-file ()
  "Self-generate this file from regenerate.rb" 
  (interactive)
  (compile-this-file-using-ruby-script "-S" "regenerate") )

(defun dev-regenerate ()
  "Self-generate this file or directory from dev version of regenerate.rb" 
  (interactive)
  (compile-this-file-using-ruby-script (concat "-I" *regenerate-dir* "/lib") 
                                       (concat *regenerate-dir* "/bin/regenerate") ) )

(defun get-this-file-or-directory-name()
  (let ( (buffer-file-name (buffer-file-name)) )
    (if (not buffer-file-name)
	(setq buffer-file-name default-directory) )
    (expand-file-name (buffer-file-name)) ) )

(defun compile-this-file-using-ruby-script (&rest ruby-script-args)
  "Compile this file or directory from RUBY-SCRIPT"
  (save-this-buffer-and-others)
  (let ( (filename (get-this-file-or-directory-name))
	 (file-buffer (current-buffer))
	 (ruby-executable (project-file :ruby-executable))
	 (ruby-args (project-value :ruby-args))
	 (base-directory (project-base-directory-value))
         (ruby-buffer (get-buffer-create "*ruby*")) )
    (message "Regenerating %s with ruby command %s ..." filename ruby-script-args)
    (display-buffer ruby-buffer)
    (save-selected-window
      (set-buffer ruby-buffer)
      (clear-buffer)
      (message "ruby-args = %s, ruby-script-args = %s, filename = %s" ruby-args ruby-script-args filename)
      (apply #'call-process 
             `(,ruby-executable nil (t t) t ,@ruby-args 
                                ,@ruby-script-args ,filename))
      (message "Finished regenerating %s" filename)
      (set-window-point (get-buffer-window ruby-buffer) (point-max)) )
    (revert-if-saved) ) )

(defun open-file-in-web-browser (file)
  "Show this file in chosen web-browser as specified by variable *web-browser-executable*"
  (interactive)
  (start-process "*web-browser*" "*web-browser*" 
		 *web-browser-executable* (concat "file:///" (expand-file-name file)))
  (switch-to-buffer-other-window "*web-browser*") )

(defun create-display-date()
  (format-time-string "%-e %B, %Y") )

(defun replace-in-buffer (from-string to-string)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward from-string nil t)
      (replace-match to-string to-string t) ) ) )

(buffer-file-name)

(defun spacify-char (ch)
  "A helper function when spacifying camel case - inserts space before a capital letter"
  (if (and (>= ch 65) (<= ch 90))
      (concat " " (char-to-string ch))
    (char-to-string ch)) )

(defun spacify-camel-case(string)
  "Given a string like 'HelloThereWorld', return 'Hello There World'"
  (let ( (first-char (substring string 0 1))
         (rest (substring string 1) ) )
    (concat first-char
            (apply 'concat (mapcar 'spacify-char rest)) ) ) )

(defun create-title-from-file-name (file-name)
  "Create an initial title for a camel-cased file name"
  (spacify-camel-case
   (file-name-sans-extension (file-name-nondirectory file-name)) ) )

(defun new-web-page()
  "Start a new empty web page from _template.html in the same directory, filling in {DATE} and {TITLE} with initial values"
  (interactive)
  (insert-file-contents "_template.html")
  (replace-in-buffer "{DATE}" (create-display-date))
  (replace-in-buffer "{TITLE}" 
                     (create-title-from-file-name (buffer-file-name)) )
  (save-buffer) )
  
(add-hook 'web-mode-hook 'web-mode-hook-function)

(load "html-abbrev")
