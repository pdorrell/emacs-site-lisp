;; Copyright (C) 2001 Philip Dorrell

(defvar file-menu-mode-map nil  "Key map for file-menu-mode")
(if file-menu-mode-map
    nil
  (setq file-menu-mode-map (make-sparse-keymap))
  )
(define-key file-menu-mode-map [f4] 'open-menu-or-file-at-point)

(defun get-file-menu-file-name (file-name)
  "Get the file-menu file name of FILE-NAME, but only if FILE-NAME ends with '/'"
  (if (string-ends-with file-name "/")
      (expand-file-name "_" file-name) ) )

(defun open-menu-or-file-at-point()
  "Open file name at point, with special case - if it's a directory ending in '/',
   open the file menu in that directory, if it exists"
  (interactive)
  (let* ( (filename-at-point (file-menu-filename-at-point))
          (file-menu-file-name 
           (if (string-ends-with filename-at-point "/")
               (expand-file-name "_" filename-at-point) ) ) )
    (if (and file-menu-file-name (file-exists-p file-menu-file-name))
        (find-file file-menu-file-name)
      (find-file filename-at-point) ) ) )
                                       
(defun file-menu-mode ()
  "Mode for file menus, ie text files with file names, written optionally in a hierarchical mode"
  (setq major-mode 'file-menu-mode
	mode-name "File Menu")
  (setq filename-at-point-function 'file-menu-filename-at-point)
  (use-local-map file-menu-mode-map) 
  )

(defun word-start-pos-in-line (line pos)
  "Find the starting position of a word in LINE containing character at POS starts,
   with a 'word' defined as a sequence of non-space characers"
  (if (>= pos (length line))
      nil
    (if (= ?  (aref line pos))
	nil
      (let ( (start pos) )
	(while (and (> start 0) (not (= ?  (aref line (1- start)))))
	  (setq start (1- start)) )
	start) ) ) )

(defun word-starting-at (line start)
  "Get the word starting at position START in LINE, where a 
   with a 'word' defined as a sequence of non-space characers"
  (let ( (end start) (len (length line)) )
    (while (and (< end len) (not (= ? (aref line end))))
      (setq end (1+ end)) )
    (substring line start end)))

(defun word-at-pos-in-line (line pos)
  "Get the word from LINE containing character at POS,
   with a 'word' defined as a sequence of non-space characers"
  (let ( (start (word-start-pos-in-line line pos)) )
    (if start
	(word-starting-at line start) ) ) )

(defun to-next-tab-stop (pos)
  "From POS in current buffer, return position of next tab stop"
  (* tab-width (1+ (/ pos tab-width))) )

(defun tabs-to-spaces (line)
  "Taking into account tab stops, convert all tabs in LINE to spaces"
  (let ( (len 0)
	 (line-len (length line)) ch out-line)
    (dotimes (i line-len)
      (setq ch (aref line i))
      (if (= ch ?\t)
	  (setq len (to-next-tab-stop len))
	(setq len (1+ len)) ) )
    (setq out-line (make-string len ? ))
    (setq len 0)
    (dotimes (i line-len)
      (setq ch (aref line i))
      (if (= ch ?\t)
	  (setq len (to-next-tab-stop len))
	(progn
	  (aset out-line len ch)
	  (setq len (1+ len)) ) ) )
    out-line) )

(defun first-non-space-pos-in-line (line)
  "Get position of first non-space character in LINE"
  (let ( (start 0) (len (length line)) )
    (while (and (< start len) (= (aref line start) ? ))
      (setq start (1+ start)))
    start) )

(run-test-check-expected-result (first-non-space-pos-in-line "   jim") 3)

(defun file-menu-read-treed-name ()
  (save-excursion
    (file-menu-read-treed-name2 (current-column)) ) )

(defun file-menu-read-treed-name2 (column)
  (let* ( (line (tabs-to-spaces (buffer-line (point))))
	  (word (word-at-pos-in-line line column)) )
    (if word
	(let ( (line-start (first-non-space-pos-in-line line)) )
	  (if (and (< line-start (length line)) (= ?| (aref line line-start)))
	      (if (find-line-with-word-at line-start)
		  (let ( (dir (file-menu-read-treed-name2 line-start)))
		    (if (string-ends-with dir "_")
			(setq dir (substring dir 0 (1- (length dir)))))
		    (concat dir word) )
		word)
	    word) ) ) ) )

(defun find-line-with-word-at (pos)
  (block loop
    (while t
      (beginning-of-line)
      (if (= (point) (point-min))
	  (return-from loop nil) )
      (forward-line -1)
      (let ( (line (tabs-to-spaces (buffer-line (point)))) )
	(if (and (< pos (length line)) (not (= ?  (aref line pos))) (not (= ?| (aref line pos))))
	    (return-from loop t) ) ) ) ) )

(defun file-menu-filename-at-point ()
  (if saved-mouse-selection
      (get-saved-mouse-selection)
    (let ( (file-name (file-menu-read-treed-name)) )
      file-name) ) )
