;; Copyright (C) 2000 Philip Dorrell

(require 'web-mode)

(setq web-mode-markup-indent-offset 2)

;; Note - this abbreviation system is not very compatible with rjsx-mode 
;; (which has its own dynamic system of tag opening and closing).

(defun def-html-abbrev (abbrev expansion)
  "Define ABBREV as an abbreviation that expands to EXPANSION for html-mode"
  (put 'html-abbreviation-expansion (intern abbrev) expansion) )

(defun def-html-abbrevs(abbrev-expansions)
  "Define a sequence of abbreviations where ABBREV-EXPANSIONS is a list of
  abbrev/expansion pairs, ie argument lists to def-html-abbrev"
  (dolist (abbrev-expansion abbrev-expansions)
    (cl-destructuring-bind (abbrev expansion) abbrev-expansion
      (def-html-abbrev abbrev expansion) ) ) )

(defun def-html-pair-abbrev (abbrev before-part after-part)
  "Define ABBREV to expand to BEFORE-PART before the point and AFTER-PART after the point"
  (let ( (expansion (list before-part 'mark after-part 'goto-mark) ) )
    (def-html-abbrev abbrev expansion) ) )

(defun def-html-pair-abbrevs(abbrev-before-part-after-part-s)
  "Define a list of abbreviations that expand to before and after parts,
ie ABBREV-BEFORE-PART-AFTER-PART-S is a list of argument lists to def-html-pair-abbrev "
  (dolist (abbrev-before-part-after-part abbrev-before-part-after-part-s)
    (cl-destructuring-bind (abbrev before-part after-part) abbrev-before-part-after-part
      (def-html-pair-abbrev abbrev before-part after-part) ) ) )

(defun get-html-abbrev-before()
  "Get abbreviation for html before point."
  (let ((start (1- (point))) (end (point)) )
    (while (buffer-char-in-table abbrev-word-table start)
      (setq start (1- start)) )
    (setq start (1+ start))
    (if (> end start)
	(buffer-substring-no-properties start end)
      nil) ) )

(defun html-wrap-selection-with-tags (before-tag after-tag)
  "Wrap selected text with BEFORE-TAG and AFTER-TAG"
  (let ( (start (region-beginning))
	 (end (region-end)) )
    (if (and start end)
	(progn
	  (goto-char start)
	  (insert before-tag)
	  (goto-char end) (forward-char (length before-tag))
	  (insert after-tag)) ) ) )

(defun html-make-bold ()
  "Wrap selected text in <b> </b> tags"
  (interactive)
  (html-wrap-selection-with-tags "<b>" "</b>") )

(defun html-make-italic ()
  "Wrap selected text in <i> </i> tags"
  (interactive)
  (html-wrap-selection-with-tags "<i>" "</i>") )

(defun html-make-ahref ()
  "Turn selected text into a link"
  (interactive)
  (html-wrap-selection-with-tags "<a href=\"\">" "</a>") )

(defun expand-html-abbrev ()
  "Expand abbreviation before point defined using set-html-abbrev"
  (interactive)
  (let ( (abbrev (get-html-abbrev-before)) expansion)
    (if abbrev
	(progn
	  (setq expansion (get 'html-abbreviation-expansion (intern abbrev)))
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

(defconst html-mode-keymap (make-sparse-keymap) "Keymap for html-mode")

(defconst html-abbrev-word-table 
  (make-alpha-table "@#abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"))

(defconst html-alpha-table
  (make-alpha-table "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_") )

(defconst xml-default-line-filtering-regex
  (make-regex '(seq start any-whitespace "<" "[^/>]+" ">") 
              "^[ \t]*<[^/>]+>")
  "Default filter for html/xml mode - any line where first non-whitespace is an opening (of self-closing) tag")

(defun web-mode-hook-function ()
  "Hook function for web-mode"
  (setq indent-tabs-mode nil)
  (use-local-map html-mode-keymap)
  (setq abbrev-word-table html-abbrev-word-table)
  (setf word-alpha-table html-alpha-table)
  (setq default-line-filtering-regex xml-default-line-filtering-regex)
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

(defun open-file-in-web-browser (file)
  "Show FILE in chosen web-browser as specified by variable *web-browser-executable*"
  (interactive)
  (start-process "*web-browser*" "*web-browser*" 
		 *web-browser-executable* (concat "file:///" (expand-file-name file)))
  (switch-to-buffer-other-window "*web-browser*") )

(add-hook 'web-mode-hook 'web-mode-hook-function)

(load "html-abbrev")
