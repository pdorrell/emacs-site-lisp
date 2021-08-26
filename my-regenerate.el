(defun create-display-date()
  (format-time-string "%-e %B, %Y") )

(defun replace-in-buffer (from-string to-string)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward from-string nil t)
      (replace-match to-string to-string t) ) ) )

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
