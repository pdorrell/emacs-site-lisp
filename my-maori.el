

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
	  (browse-url (maori-dictionary-search-url word)) )
      (message "No word at point") ) ) )

(defun maori-dictionary-search-url(maori-word)
  (concat "http://www.maoridictionary.co.nz/index.cfm?dictionaryKeywords=" maori-word) )

(global-set-key [?\C-\M-l] 'maori-lookup-word-in-dictionary)
