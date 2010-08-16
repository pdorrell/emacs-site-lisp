(add-hook 'lisp-mode-hook 'my-lisp-hook-function)

(defun show-inferior-lisp-buffer ()
  (interactive)
  (display-buffer "*inferior-lisp*") )

(defun my-lisp-hook-function()
  (font-lock-mode 1)
  (setq word-alpha-table emacs-lisp-word-alpha-table)
  (setq expansion-key 'lisp-expansion-key)
  (local-set-key [f9] 'lisp-eval-defun-and-show)
  (local-set-key [C-M-f9] 'lisp-eval-buffer-and-show)
  (local-set-key [M-f9] 'quit-lisp-break)
  (local-set-key [?\M-k] 'show-inferior-lisp-buffer)
  (local-set-key [?\M-N] 'plisp-preview)
  (local-set-key [?\C-\M-n] 'lisp-setup)
  (local-set-key [?\M-p] 'plisp-setup)
  (local-set-key [f2] 'my-expand-abbrev)
  (local-set-key [?\C-w] 'lisp-search-for-identifier-at-point)
  (local-set-key [?\C-m] 'return-and-indent)
  (setq comment-start ";;") ; Single semi-colon not for comment on new line
  (setq comment-end "")
)

(defun run-inferior-lisp ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (run-lisp inferior-lisp-program)
  (local-set-key [?\M-K] 'bury-buffer) )

(global-set-key [?\C-\M-l] 'run-inferior-lisp)

(defun show-inferior-lisp-buffer-at-end ()
  (display-buffer "*inferior-lisp*" t)
  (save-current-buffer
    (set-buffer "*inferior-lisp*")
    (let ( (inferior-lisp-buffer (current-buffer))
	   (current-window (selected-window)) )
      (walk-windows
       #'(lambda (window)
		   (if (eq (window-buffer window) inferior-lisp-buffer)
		       (progn 
			 (select-window window)
			 (goto-char (point-max))
			 (select-window current-window) ) ) ) ) ) ) )

(defun inferior-lisp-command (command)
  (comint-send-string (inferior-lisp-proc) (format "%S\n" command)) )

(defun plisp-preview ()
  (interactive)
  (save-buffer)
  (save-some-buffers)
  (if (not plisp-has-been-setup) 
      (plisp-setup) )
  (show-inferior-lisp-buffer-at-end)
  (let ( (base-name (file-name-minus-extension (buffer-name))) )
    (inferior-lisp-command (list 'ps-and-show 
				 (concat default-directory (buffer-name))
				 (concat default-directory "output/" base-name ".eps") ) ) ) )

(defun lisp-eval-defun-and-show ()
  (interactive)
  (show-inferior-lisp-buffer-at-end)
  (lisp-eval-defun) )

(defun lisp-eval-buffer-and-show ()
  (interactive)
  (save-buffer)
  (save-some-buffers)
  (show-inferior-lisp-buffer-at-end)
  (inferior-lisp-command `(progn (setf (default-directory) ,default-directory)
				 (load ,(buffer-file-name))) ) )

(defun quit-lisp-break ()
  (interactive)
  (show-inferior-lisp-buffer-at-end)
  (comint-send-string (inferior-lisp-proc) "quit\n") )

(defun lisp-setup ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (run-lisp inferior-lisp-program)
  (inferior-lisp-command `(setf (default-directory) ,(project-directory :lisp-default-directory)))
  (local-set-key [?\M-K] 'bury-buffer) )

(defvar plisp-has-been-setup nil)

(defun plisp-setup ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (run-lisp inferior-lisp-program)
  (inferior-lisp-command `(setf (default-directory) ,(project-directory :lisp-default-directory)))
  (inferior-lisp-command '(load "build"))
  (local-set-key [?\M-K] 'bury-buffer)
  (setf plisp-has-been-setup t) )

(setq inferior-lisp-program "clisp -K full")

(put 'unit-tests 'lisp-indent-function 1)
(put 'for-exprs-in-file 'lisp-indent-function 1)
(put 'with-macro-vars 'lisp-indent-function 1)
(put 'with-slots 'lisp-indent-function 1)
(put 'html-file-page 'lisp-indent-function 1)
(put 'glossary-section 'lisp-indent-function 1)
(put 'do-table-items 'lisp-indent-function 1)
(put 'do-table-items-in-date-range 'lisp-indent-function 1)

(set-lisp-abbrev "me" "(macroexpand-1 '")

(set-lisp-abbrev "pr" '("(format t \"" mark "~%\")" goto-mark))

