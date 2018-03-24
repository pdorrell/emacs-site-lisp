;; Copyright (C) 2000,2001 Philip Dorrell

;-----------------------------------------------------------------
(defun return-and-indent ()
  "Insert new line and indent"
  (interactive)
  (newline)
  (indent-for-tab-command) )

(defun insert-space-if-not-there()
  (let* ( (point (point))
          (prev-char (char-after (- point 1))) )
    (if (not (= prev-char 32))
        (insert " ")) ) )

(defun set-extension-mode (extension mode)
  (setq auto-mode-alist
        (cons (cons (concat "\\" extension "\\'") mode)
              auto-mode-alist) ) )

(defun set-file-name-mode (file-name mode)
  (setq auto-mode-alist
        (cons (cons (concat "[/]" (regexp-quote file-name) "\\'") mode)
              auto-mode-alist) ) )
;-----------------------------------------------------------------
(defun text-hook ()
  (local-unset-key [?\M-S]) )

(setq text-mode-hook '(text-hook))

(setq filter-regexp (make-regexp '(seq (at-least-once (not-set " \t\n")))))
(make-variable-buffer-local 'filter-regexp)
(make-variable-buffer-local 'filtering-minor-mode)

(add-to-list 'minor-mode-alist '(filtering-minor-mode " Filt"))

(defun toggle-filtering ()
  "If alreading filtering, turn it off, if not, then filter on FILTER-REGEXP"
  (interactive)
  (if filtering-minor-mode
      (let ( (saved-modified (buffer-modified-p)) )
        (put-text-property (point-min) (point-max) 'invisible nil)
        (set-buffer-modified-p saved-modified ) ) 
    (filter-on-line-match (concat "^\\(" filter-regexp "\\)") ) )
  (setq filtering-minor-mode (not filtering-minor-mode)) )

(defun filter-set-window-start ()
  (let ( (w (get-buffer-window (current-buffer))) 
         (p (save-excursion (beginning-of-line) (point))) )
    (recenter (- (window-height w) 2)) ) )

(defun filter-on-line-match (line-match-regexp)
  (let ( (saved-modified (buffer-modified-p)) )
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
  (let ( (saved-modified (buffer-modified-p)) )
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

(defun describe-function-at-pos ()
  (interactive)
  (let ( (word (word-at emacs-lisp-word-alpha-table (point))) )
    (if word
        (describe-function (intern word))
      (call-interactively 'describe-function) ) ) )
    
(defun describe-variable-at-pos ()
  (interactive)
  (let ( (word (word-at emacs-lisp-word-alpha-table (point))) )
    (if word
        (describe-variable (intern word))
      (call-interactively 'describe-variable) ) ) )

(defun start-or-end-kbd-macro ()
  "If defining a keyboard macro, finish it, else start one"
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil) ) )

;;-----------------------------------------------------------------

(defun clear-buffer ()
  "Clear buffer not visiting file."
  (interactive)
  (if (buffer-file-name)
      (message "Cannot clear a buffer visiting a file")
    (progn
        (delete-region (point-min) (point-max))
        (if (memq major-mode '(shell-mode rlogin-mode))
            (comint-send-input) ) ) ) )

(global-set-key [?\M-C] 'clear-buffer)

(defun visit-scratch-buffer()
  (interactive)
  (show-buffer nil "*scratch*") )

(defun revert-if-saved()
  (interactive)
  (if (buffer-modified-p)
      (message "Buffer has been modified, save it first")
    (progn
      (revert-buffer t t)
      (message "Buffer reverted") ) ) )

(defun shift-initial-case ()
  (interactive)
  (let* ( (pos (point))
          (word (word-before java-word-table pos))
          (letter-pos 0) ch)
    (if word
        (progn
          (if (eql pos last-paste-end)
              (setq letter-pos 
                    (+ (length word) (- last-paste-start last-paste-end))) )
          (setq ch (aref word letter-pos))
          (delete-backward-char (length word))
          (if (and (>= ch ?a) (<= ch ?z))
              (setq ch (- ch 32))
            (if (and (>= ch ?A) (<= ch ?Z))
                (setq ch (+ ch 32)) ) )
          (aset word letter-pos ch)
          (insert word) ) ) ) )

;-----------------------------------------------------------------
(global-set-key [delete] 'kill-region)
(global-set-key [insert] 'yank)
(global-set-key [?\C-u] 'undo)
(global-set-key "\M-n" 'revert-if-saved)

(global-set-key [?\M--] 'other-window)
(global-set-key [?\M-B] 'buffer-menu)
(global-set-key [?\M-o] 'filter-on-word-at-point)
(global-set-key [?\M-O] 'toggle-filtering)
(global-set-key [?\M-I] 'filter-on-indent)
(global-set-key [C-f6] 'start-or-end-kbd-macro)

(global-set-key [S-C-f11] 'describe-key)
(global-set-key [S-f11] 'describe-function-at-pos)
(global-set-key [C-f11] 'describe-variable-at-pos)
(global-set-key [S-C-f11] 'describe-key)

(global-set-key [?\M-\C-b] 'visit-scratch-buffer)


(global-set-key [f12] 'repeat-complex-command)

(filter-set-window-start)

(global-set-key [?\C-t] 'toggle-truncate-lines)
(global-set-key [?\C-\M-t] 'toggle-truncate-lines)

;; For emacs 24, re-instate auto-copy when selecting region with mouse
(setq mouse-drag-copy-region t)

(set-face-attribute 'region nil :background "#ffff00" :foreground "#000")

