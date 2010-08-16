;; Copyright (C) 2000 Philip Dorrell

;;========================================================================
(defun thin-comment-line ()
  "Insert thin-comment"
  (interactive)
  (beginning-of-line)
  (if comment-start (insert comment-start))
  (dotimes (i 65) (insert "-"))
  (if comment-end (insert comment-end))
  (newline) )

(defun thick-comment-line ()
  "Insert thin-comment"
  (interactive)
  (beginning-of-line)
  (if comment-start (insert comment-start))
  (dotimes (i 72) (insert "="))
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
  (if (re-search-forward "^....[=-][=-][=-][=-][=-]" nil t)
      (progn
	(beginning-of-line)
	(recenter 0) )
    (message "No more comment lines") ) )

;;-----------------------------------------------------------------
(defun find-comment-line-bwd()
  "Find comment line backward"
  (interactive)
  (forward-char -1)
  (if (re-search-backward "^....[=-][=-][=-][=-][=-]" nil t)
      (progn
	(beginning-of-line)
	(recenter 0) )
    (message "No more comment lines") ) )

;;========================================================================
(global-set-key [?\M-L] 'thin-comment-line)
(global-set-key [?\M-T] 'thick-comment-line)

(global-set-key [M-kp-down] 'find-comment-line-fwd)
(global-set-key [M-down] 'find-comment-line-fwd)

(global-set-key [M-kp-up] 'find-comment-line-bwd)
(global-set-key [M-up] 'find-comment-line-bwd)

(global-set-key [M-kp-next] 'find-thick-comment-line-fwd)
(global-set-key [M-next] 'find-thick-comment-line-fwd)

(global-set-key [M-kp-prior] 'find-thick-comment-line-bwd)
(global-set-key [M-prior] 'find-thick-comment-line-bwd)

