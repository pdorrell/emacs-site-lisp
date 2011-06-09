;; Copyright (C) 2000,2001 Philip Dorrell

(defun start-killable-shell()
  "Start a shell that can be killed no questions asked (unless win9x)"
  (interactive)
  (shell)
  (if (not running-windows-9x)
      (process-kill-without-query 
       (get-buffer-process (current-buffer)) ) ) )

(global-set-key [?\M-S] 'start-killable-shell)
(global-set-key [?\M-\C-S] 'show-shell-this-dir)

(defun run-command-in-shell (command)
  "Run command in shell"
  (end-of-buffer)
  (insert command)
  (comint-send-input) )

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

(defun show-shell-this-dir ()
  "Bring up shell and set to this directory"
  (interactive)
  (let ( (new-dir (windowize-filename default-directory)) (save-dir default-directory))
    (start-killable-shell)
    (if running-windows
	(let ( (drive (get-filename-drive new-dir)) )
	  (if drive
	      (run-command-in-shell (concat drive ":")) ) ) )
    (run-command-in-shell (concat "cd \"" new-dir "\""))
    (setq default-directory save-dir) ) )


(defun show-file-and-shell-this-dir ()
  "Find file at point and bring up shell and set to same directory"
  (interactive)
  (find-file-at-point)
  (show-shell-this-dir) )

(setq shell-mode-hook '(shell-hook))

(setq *shell-dirtrack-regexp*
      (if running-windows
	  (make-regexp '(seq start (paren (set "a-zA-Z") ":" (repeated any)) ">"))
	 "\\(.+\\)> $") )

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
  (show-completion-buffer-with-string "*shell-history*" (concat-lines (shell-history)) 'put-command-in-shell) )


(defun shell-hook ()
  (local-set-key [?\M-H] 'show-history-list)
  (local-set-key [C-pause] 'comint-interrupt-subjob)
  (setq filter-regexp shell-filter-regexp)
  (setq word-alpha-table filename-word-table)
  (setq shell-dirtrackp nil)
  (setq dirtrack-list (list *shell-dirtrack-regexp* 1 nil))
  (shell-dirtrack-mode -1)
  (dirtrack-mode) )
;;========================================================================
(global-set-key [C-M-S-f4] 'show-file-and-shell-this-dir)
