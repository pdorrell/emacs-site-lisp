;;-----------------------------------------------------------------
(global-set-key [?\M-B] 'buffer-menu)

(global-set-key [?\M-C] 'clear-buffer)

(global-set-key [?\M-d] 'delete-word-at-point)
(global-set-key [?\M-\C-d] 'open-this-directory)
(global-set-key [?\M-D] 'dired-default-directory)

(global-set-key [?\C-e] 'sql-execute-query-at-point)

(global-set-key [?\M-F] 'font-lock-mode)
(global-set-key [?\M-\C-F] 'show-face-at-point)

(global-set-key [?\C-\M-g] 'goto-line)
(global-set-key [?\M-G] 'git-gui-this-directory)

(global-set-key [?\M-I] 'filter-on-indent)

(global-set-key [?\M-K] 'kill-buffer-y-or-n)

(global-set-key [?\M-L] 'thin-comment-line)

(global-set-key [?\M-M] 'build-project)
(global-set-key [?\C-\M-M] 'build-project-with-target)

(global-set-key [?\M-n] 'revert-if-saved)

(global-set-key [?\M-o] 'filter-on-word-at-point)
(global-set-key [?\M-O] 'toggle-filtering)
(global-set-key [?\C-\M-o] 'show-project-log-buffer)

(global-set-key [?\M-p] 'visit-project-file)
(global-set-key [?\M-P] 'show-position)

(global-set-key [?\M-Q] 'indent-sexp)

(global-set-key [?\M-S] 'start-killable-shell)

(global-set-key [?\C-t] 'toggle-truncate-lines)
(global-set-key [?\C-\M-t] 'toggle-truncate-lines)
(global-set-key [?\M-T] 'thick-comment-line)

(global-set-key [?\C-u] 'undo)

(global-set-key [?\C-w] 'search-this-dir)

(global-set-key [?\M-z] 'show-messages-buffer)

(global-set-key [?\M--] 'other-window)
;;-----------------------------------------------------------------
(global-set-key "\C-xK" 'kill-buffer-y-or-n)

;;-----------------------------------------------------------------
(global-set-key [f2] 'my-expand-abbrev)

(global-set-key [f3] 'save-buffer)
(global-set-key [S-f3] 'save-and-kill-buffer)
(global-set-key [M-f3] 'save-this-buffer-and-others)

(global-set-key [f4] 'find-file-at-point)
(global-set-key [C-f4] 'find-file-at-point-full-window)
(global-set-key [S-f4] 'visit-file-line)
(global-set-key [M-f4] 'run-file-at-point)
(global-set-key [S-M-f4] 'execute-this-file)

(global-set-key [f5] 'save-mouse-selection)

(global-set-key [C-f6] 'start-or-end-kbd-macro)
(global-set-key [S-f6] 'call-last-kbd-macro)

(global-set-key [f7] 'open-base-file-menu)
(global-set-key [C-f7] 'edit-emacs-customisation)
(global-set-key [C-M-f7] 'open-project-file-menu)
(global-set-key [C-S-f7] 'visit-sql-file)
(global-set-key [M-f7] 'open-file-menu)
(global-set-key [S-f7] 'open-file-menu-other-window)
(global-set-key [S-M-f7] 'open-base-file-menu-other-window)

(global-set-key [M-f8] 'run-alternate-command)

(global-set-key [M-f9] 'run-this-file)
(global-set-key [C-M-f9] 'run-this-file)
(global-set-key [S-C-f9] 'kill-buffer-process)
(global-set-key [S-M-f9] 'run-project)

(global-set-key [C-f10] 'set-dual-frames)
(global-set-key [C-M-f10] 'comment-region)
(global-set-key [S-M-f10] 'gitk-this-directory)

(global-set-key [C-f11] 'describe-variable-at-pos)
(global-set-key [S-f11] 'describe-function-at-pos)
(global-set-key [S-C-f11] 'describe-key)

(global-set-key [f12] 'repeat-complex-command)
(global-set-key [M-f12] 'project-search-for-identifier-at-point)

;;-----------------------------------------------------------------
(global-set-key [M-S-down] 'point-to-bottom)
(global-set-key [M-S-kp-down] 'point-to-bottom)

(global-set-key [home] 'beginning-of-line)
(global-set-key [C-home] 'beginning-of-buffer)

(global-set-key [end] 'end-of-line)
(global-set-key [C-end] 'end-of-buffer)

(global-set-key [C-kp-left] 'scroll-left-a-bit)
(global-set-key [M-left] 'backward-sexp)

(global-set-key [C-kp-right] 'scroll-right-a-bit)
(global-set-key [M-right] 'forward-sexp)

(global-set-key [C-kp-up] 'scroll-up-one)
(global-set-key [M-up] 'find-comment-line-bwd)
(global-set-key [M-S-up] 'point-to-top)

(global-set-key [C-kp-down] 'scroll-down-one)

(global-set-key [M-down] 'find-comment-line-fwd)

;;-----------------------------------------------------------------
(global-set-key [M-next] 'find-thick-comment-line-fwd)

(global-set-key [M-prior] 'find-thick-comment-line-bwd)

;;-----------------------------------------------------------------
(global-set-key [pause] 'next-error)
(global-set-key [S-pause] 'previous-error) 

(global-set-key [delete] 'kill-region)
(global-set-key [insert] 'yank)
;;-----------------------------------------------------------------
(global-set-key [mouse-3] 'paste-word)
(global-set-key [C-S-mouse-3] 'paste-line)
(global-set-key [M-mouse-3] 'paste-filename)
(global-set-key [S-mouse-3] 'replace-word)


;;-----------------------------------------------------------------
;;(global-set-key [?\M-\C-S] 'spanish-minor-mode)
;;(global-set-key [f8] 'spanish-toggle-accent)
;;(global-set-key [?\C-\M-l] 'maori-lookup-word-in-dictionary)

