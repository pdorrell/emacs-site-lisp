;; Copyright (C) 2000,2001 Philip Dorrell

(defun start-killable-shell()
  "Start a shell that can be killed no questions asked (unless win9x)"
  (interactive)
  (shell)
  (process-kill-without-query 
   (get-buffer-process (current-buffer)) ) )

(defun put-command-in-shell (command)
  "Put command in shell ready to be sent"
  (end-of-buffer)
  (delete-region 
   ;; Can't use kill-region as it sets this-command
   (or  (marker-position comint-accum-marker)
	(process-mark (get-buffer-process (current-buffer))))
   (point))
  (insert command) )

(defun char-pos (ch string)
  (let ( (len (length string))
	 (i 0) (found nil))
    (while (and (< i len) (not found))
      (if (= ch (aref string i))
	  (setq found t)
	(setq i (1+ i))) )
    (if found i -1) ) )

(defun get-filename-drive (filename)
  (let ( (colon-pos (char-pos ?: filename)) )
    (if (= colon-pos -1)
	nil
      (progn
	(substring filename 0 colon-pos) ) ) ) )

(setq shell-mode-hook '(shell-hook))

(defconst *shell-dirtrack-regexp* "\\(?:\\[[^]]*\\] \\)*\\(.+\\)> $" "regex used for dirtrack-list & shell-dirtrack-mode")

(setq shell-filter-regexp
      (make-regexp '(seq (set "a-zA-Z") ":" (repeated (set "-a-zA-Z0-9_.\\")) ">" )
		   ) )

(defun shell-history ()
  (let ( (len (ring-length comint-input-ring)) (history nil) )
    (dotimes (i len)
      (setq history (cons (ring-ref comint-input-ring i) history)) )
    history) )

(defun concat-lines (lines)
  (let ( (result "") )
    (dolist (line lines)
      (setq result (concat result line "\n")) )
    result) )

(defun show-history-list ()
  (interactive)
  (show-completion-buffer-with-string 
   "*deduped-shell-history*" 
   (concat-lines (reverse (delete-dups (reverse (shell-history)))))
   'put-command-in-shell) )


(defun shell-hook ()
  (local-set-key [?\M-H] 'show-history-list)
  (local-set-key [C-pause] 'comint-interrupt-subjob)
  (setq filter-regexp shell-filter-regexp)
  (setq word-alpha-table filename-word-table)
  (setq shell-dirtrackp nil)
  (setq dirtrack-list (list *shell-dirtrack-regexp* 1 nil))
  (shell-dirtrack-mode -1)
  (dirtrack-mode) )
