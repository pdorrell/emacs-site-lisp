;; Copyright (C) 2000 Philip Dorrell

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

(defun html-helper-mode-hook-function ()
  (use-local-map html-mode-keymap)
  (setq abbrev-word-table html-abbrev-word-table)
  (setf word-alpha-table html-alpha-table)
  (setq filter-regexp xml-filter-regexp)
  (local-set-key [f10] 'shift-initial-case)
  (local-set-key [?\M-\C-\ ] "&nbsp;")
  (local-set-key [f2] 'expand-html-abbrev)
  (local-set-key [S-f5] 'make-wiki-link) 
  (local-set-key [M-C-f8] 'self-generate-this-file)
  (setq run-file-function #'firefox-file)
  (local-set-key [?\M-e] 'edit-this-file)
  (local-set-key [?\C-b] 'html-make-bold)
  (local-set-key [?\M-\C-i] 'html-make-italic)
  (local-set-key [?\C-a] 'html-make-ahref)
  (font-lock-mode 1)
  (setq file-editor *external-html-editor*)
  )

(defun rejinnirate-this-file ()
  "Self-generate this file from _pages.rb"
  (interactive)
  (save-this-buffer-and-others)
  (let ( (filename (expand-file-name (buffer-file-name)))
	 (file-buffer (current-buffer))
	 (ruby-executable (project-file :ruby-executable))
	 (ruby-args (project-value :ruby-args))
	 (rejinnirate-directory (project-file :rejinnirate-directory)) )
    (message "Rejinnirating %s ..." filename)
    (switch-to-buffer-other-window "*ruby*" t)
    (clear-buffer)
    (apply #'call-process 
	   `(,ruby-executable nil "*ruby*" t ,@ruby-args 
	     ,(concat rejinnirate-directory "/rejinnirate-run.rb") ,filename))
    (goto-char (point-max))
    (message "Finished rejinnirating %s" filename)
    (save-excursion
      (set-buffer file-buffer) 
      (revert-if-saved) ) ) )

(defun rejenner-this-file ()
  "Self-generate this file from _rejenner.rb" 
  (interactive)
  (regenerate-this-file-using-ruby-script (concat base-directory "src/_rejenner.rb")) )

(defun regenerate-this-file ()
  "Self-generate this file from regenerate.rb" 
  (interactive)
  (regenerate-this-file-using-ruby-script "-S" "regenerate") )

(defun regenerate-this-file-using-ruby-script (&rest ruby-script-args)
  "Self-generate this file from RUBY-SCRIPT"
  (save-this-buffer-and-others)
  (let ( (filename (expand-file-name (buffer-file-name)))
	 (file-buffer (current-buffer))
	 (ruby-executable (project-file :ruby-executable))
	 (ruby-args (project-value :ruby-args))
	 (base-directory (project-base-directory-value)) )
    (message "Regenerating %s with ruby command %s ..." filename ruby-script-args)
    (switch-to-buffer-other-window "*ruby*" t)
    (clear-buffer)
    (apply #'call-process 
	   `(,ruby-executable nil "*ruby*" t ,@ruby-args 
	     ,@ruby-script-args ,filename))
    (goto-char (point-max))
    (message "Finished regenerating %s" filename)
    (save-excursion
      (set-buffer file-buffer) 
      (revert-if-saved) ) ) )

(defun self-generate-this-file ()
  "Self-generate this file using :self-generate-command project value"
  (interactive)
  (apply (project-value :self-generate-command) '()) )

(defun firefox-file (file)
  "Show this file in firefox"
  (interactive)
  (start-process "*firefox*" "*firefox*" 
		 *firefox-executable* (concat "file:///" (expand-file-name file)))
  (switch-to-buffer-other-window "*firefox*") )
  
(add-hook 'html-helper-mode-hook 'html-helper-mode-hook-function)

(set-extension-mode ".ftl" 'html-helper-mode)
;;(set-extension-mode ".xml" 'html-helper-mode)
(set-extension-mode ".rxml" 'html-helper-mode)
(set-extension-mode ".html" 'html-helper-mode)
(set-extension-mode ".rhtml" 'html-helper-mode)
(set-extension-mode ".html.erb" 'html-helper-mode)

(load "html-abbrev")

(setq html-helper-build-new-buffer nil)

;; New highlighting (an altered version of what's in html-helper-mode.el)
(setq html-helper-font-lock-keywords
    (list		
     ;; Avoid use of `keep', since XEmacs will treat it the same as `t'.
     ;; First fontify the text of a HREF anchor.  It may be overridden later.
     ;; Anchors in headings will be made bold, for instance 
     '("<a\\s-+href[^>]*>\\([^>]+\\)</a>"
       1 font-lock-warning-face t)
     ;; Underline is rarely used. Only handle it when no tags inside.
     '("<u>\\([^<]*\\)</u>" 1 html-helper-underline-face t)
     '(html-helper-match-bold
	   0 'html-helper-bold-face t)
     ;; Italic 
     '(html-helper-match-italics
	   0 'html-helper-italic-face t)
     ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside.
     '("<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>"
       0 font-lock-keyword-face t)
     ;; Paint [PA][HS]P skripts in font-lock-builtin-face,
     '("<[?%]=\\([^%?]\\|[?%][^>]\\)*[%?]>" 0 html-helper-builtin-face t t)
     '(html-helper-match-asp-php 0 html-helper-builtin-face t t)
     ;; after painting strings, you have to restore asp stuff inside strings
     '("\\(<%=\\w\\)" 1 html-helper-builtin-face t)
     '("\\(\")[^\"\n]*%>\\)" 1 html-helper-builtin-face t)
     '("\\(<%=[^%]*%>\\)" 1 html-helper-builtin-face t)
     '("\\(<\\?=\\w\\)" 1 html-helper-builtin-face t)
     '("\\(\")[^\"\n]*\\?>\\)" 1 html-helper-builtin-face t)
     '("\\(<\\?=[^%]*\\?>\\)" 1 html-helper-builtin-face t)
     ;; Comment declarations according to the HTML 2.0 spec at
     ;; <URL:http://www.w3.org/pub/WWW/MarkUp/html-spec/html-spec_3.html>.
     ;; Usually `<!-- ... -->', but also e.g the single, complete declaration
     ;; `<!--c1--  -- c2 -- -->c3 (still comment) ----c4- c4--   >'.
     ;; Note that e.g. Netscape 3.01 Gold doesn't fully live up to the spec.
     
     ;; That's krazy, strings higlight matches ) too, so i paint
     ;; parantheses...
     '("\\(<%\\|\\s(\\)" 1 font-lock-function-name-face t)
     '("\\(\\s)\\|%>\\)" 1 font-lock-function-name-face t)
     '("\\(<\\?\\|\\s(\\)" 1 html-tag-face t)
     '("\\(\\s)\\|\\?>\\)" 1 html-tag-face t)
     ;; w3 org says that a tag is <element-name> not < element-name>
     ;; I don't know of any non alphabetic HTML entity, if you know
     ;; about one, please drop me a mail
     ;;						Saint
     '("\\(</?[A-Za-z0-9]+\\)" 1 html-tag-face t)
     '(html-helper-match-attributes 0 font-lock-keyword-face t t)
     '("\\([\"]\\)" 0 font-lock-string-face t)
     ;; string stuff is pretty weird with asp. You can have strings
     ;; containing asp code containing strings and empty
     ;; strings. Replaced original [^\"] with this one...
     '("[=(&]?[ \t\n]*\\(\"[^\"\n]*<%[^\"\n]*\\(\"[^\"\n]*\"\\)[^\"\n]*%>[^\"\n]*\\)" 1 font-lock-string-face t)
     '("[=(&]?[ \t\n]*\\(\"[^\"\n]*\"\\)"  1 font-lock-string-face t)
     ;; FTL stuff
     '("\\(</?\\#[A-Za-z0-9]+[^>]*>\\)" 1 ftl-tag-face t)
     '("\\(</?\\@[A-Za-z0-9]+[^>]*>\\)" 1 ftl-macro-tag-face t)
     '("\\([$#]{[^}]*}\\)" 1 ftl-expr-face t)
     ;; A Regexp doesn't work well with big blocks...
     ;;      '("<!--\\(.\\|[\n]\\--[ \t]*[^>]\\)*--[ \t]*>" 0 
     ;;	font-lock-comment-face t)))
     '(html-helper-match-comments 0 font-lock-comment-face t t)
     ;; HTML special characters
     '("&[a-zA-Z0-9#]+;" 0 font-lock-warning-face t)) )
