

;; English letters + Maori letters with macrons
(defvar english-maori-letters-string
  (concat "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	  "abcdefghijklmnopqrstuvwxyz"
	  "ĀāĒēĪīŌōŪū") )

(defvar english-maori-word-table (make-alpha-table english-maori-letters-string))

(defun maori-lookup-word-in-dictionary()
  (interactive)
  (let ( (word (word-at english-maori-word-table (point))) )
    (if word
	(progn
	  (message "Looking up word %s in Maori dictionary ..." word)
	  (dolist (query-url *maori-dictionary-query-urls*)
	    (let ( (url (concat query-url word)) )
	      (message " browsing %s ..." url)
	      (browse-url url) ) ) )
      (message "No word at point") ) ) )
