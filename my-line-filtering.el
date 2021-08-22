;;-----------------------------------------------------------------
;;Line filtering

(setq filter-regexp (make-regexp '(seq (at-least-once (not-set " \t\n")))))
(make-variable-buffer-local 'filter-regexp)
(make-variable-buffer-local 'filtering-minor-mode)

(add-to-list 'minor-mode-alist '(filtering-minor-mode " Filt"))

(defun toggle-filtering ()
  "If alreading filtering, turn it off, if not, then filter on FILTER-REGEXP"
  (interactive)
  (let ( (buffer-read-only nil) )
    (if filtering-minor-mode
        (let ( (saved-modified (buffer-modified-p)) )
          (put-text-property (point-min) (point-max) 'invisible nil)
          (set-buffer-modified-p saved-modified ) ) 
      (filter-on-line-match (concat "^\\(" filter-regexp "\\)") ) )
    (setq filtering-minor-mode (not filtering-minor-mode)) ) )

(defun filter-set-window-start ()
  (let ( (w (get-buffer-window (current-buffer))) 
         (p (save-excursion (beginning-of-line) (point))) )
    (recenter (- (window-height w) 2)) ) )

(defun filter-on-line-match (line-match-regexp)
  (let ( (buffer-read-only nil) 
         (saved-modified (buffer-modified-p)) )
    (save-excursion
      (let ( line-start line-end )
        (beginning-of-line) (setq current-line-start (point))
        (if (> current-line-start (point-min)) (setq current-line-start (1- current-line-start)))
        (end-of-line) (setq current-line-end (point))
        (goto-char (point-min))
        (let ( (start (point)) end-of-previous line-start line-end)
          (while (search-forward-regexp line-match-regexp nil t)
            (beginning-of-line) (setq line-start (point))
            (end-of-line) (setq line-end (point))
            (setq end-of-previous (1- line-start))
            (if (> end-of-previous start)
                (put-text-property start end-of-previous 'invisible t) )
            (setq start line-end) )
          (put-text-property start (point-max) 'invisible t) ) 
        (put-text-property current-line-start current-line-end 'invisible nil)) )
    (filter-set-window-start)
    (set-buffer-modified-p saved-modified ) ) ) 

(defun filter-on-word-at-point ()
  (interactive)
  (let ( (word (word-at word-alpha-table (point))) )
    (if word
        (filter-on-word word)
      (call-interactively 'filter-on-word) ) ) )

(defun set-filter-on-line-match (regex)
  (message "Setting line filter to %s" regex)
  (let ( (buffer-read-only nil)
         (saved-modified (buffer-modified-p)) )
    (if filtering-minor-mode
        (put-text-property (point-min) (point-max) 'invisible nil) )
    (filter-on-line-match regex)
    (setq filtering-minor-mode t) 
    (set-buffer-modified-p saved-modified ) ) )

(defun filter-on-word (word)
  (interactive "sMatch word: ")
  (set-filter-on-line-match (regexp-quote word) ) )

(defun filter-on-indent()
  (interactive)
  (let (indent-string)
    (save-excursion
      (search-backward-regexp "^")
      (if (looking-at "[ \t]*")
          (setq indent-string (buffer-substring (match-beginning 0)
                                                (match-end 0)))
        (message "indent-string=#%s#" indent-string) ) )
    (if indent-string
        (let ( (indent-length (length indent-string)) )
          (set-filter-on-line-match 
           (format "^[ ]\\{0,%s\\}[^ ]" indent-length) ) ) ) ) )

(setq filtering-minor-mode-map (make-sparse-keymap))

(defun previous-visible-line ()
  (interactive)
  (let ( (n 1) found)
    (while (not found)
      (save-excursion
        (previous-line n)
        (if (not (get-char-property (1- (point)) 'invisible))
            (setq found t)
          (setq n (1+ n)) ) ) )
    (previous-line n) ) )

(defun next-visible-line ()
  (interactive)
  (let ( (n 1) found)
    (while (not found)
      (save-excursion
        (next-line n)
        (if (not (get-char-property (1- (point)) 'invisible))
            (setq found t)
          (setq n (1+ n)) ) ) )
    (next-line n) ) )

(defun next-visible-char ()
  (interactive)
  (forward-char)
  (while (get-char-property (point) 'invisible)
    (forward-char) ) )

(defun previous-visible-char ()
  (interactive)
  (backward-char)
  (while (get-char-property (point) 'invisible)
    (backward-char) ) )

(define-key filtering-minor-mode-map [up] 'previous-visible-line)
(define-key filtering-minor-mode-map [down] 'next-visible-line)
(define-key filtering-minor-mode-map [right] 'next-visible-char)
(define-key filtering-minor-mode-map [left] 'previous-visible-char)

(or (assq 'filtering-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'filtering-minor-mode filtering-minor-mode-map) minor-mode-map-alist)))
