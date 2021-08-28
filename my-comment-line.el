;; Copyright (C) 2000 Philip Dorrell

;;========================================================================
(defun thin-comment-line ()
  "Insert thin-comment"
  (interactive)
  (beginning-of-line)
  (if comment-start (insert comment-start))
  (dotimes (i 80) (insert "-"))
  (if comment-end (insert comment-end))
  (newline) )

(defun thick-comment-line ()
  "Insert thin-comment"
  (interactive)
  (beginning-of-line)
  (if comment-start (insert comment-start))
  (dotimes (i 100) (insert "="))
  (if comment-end (insert comment-end))
  (newline) )

(defun find-thick-comment-line-fwd()
  "Find thick comment line forward"
  (interactive)
  (forward-char)
  (if (search-forward (concat comment-start "=====") nil t)
      (progn
	(beginning-of-line)
	(recenter 0) )
    (message "No more thick comment lines") ) )

;;-----------------------------------------------------------------
(defun find-thick-comment-line-bwd()
  "Find thick comment line backward"
  (interactive)
  (forward-char -1)
  (if (search-backward (concat comment-start "=====") nil t)
   (progn (beginning-of-line) (recenter 0) )
   (message "No more thick comment lines") ) )

;;-----------------------------------------------------------------
(defun find-comment-line-fwd()
  "Find comment line forward"
  (interactive)
  (forward-char)
  (if (re-search-forward "^......[=-][=-][=-][=-][=-]" nil t)
      (progn
	(beginning-of-line)
	(recenter 0) )
    (message "No more comment lines") ) )

;;-----------------------------------------------------------------
(defun find-comment-line-bwd()
  "Find comment line backward"
  (interactive)
  (forward-char -1)
  (if (re-search-backward "^......[=-][=-][=-][=-][=-]" nil t)
      (progn
	(beginning-of-line)
	(recenter 0) )
    (message "No more comment lines") ) )

;;========================================================================

