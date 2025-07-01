(defun set-keyboard-shortcuts (label shortcuts)
  (declare (indent defun))
  (dolist (shortcut shortcuts)
    (apply 'global-set-key shortcut) )
    (message "Set %S %S keyboard shortcuts" (length shortcuts) label) )

;; most occurrences of global-set-key should occur in this file

(set-keyboard-shortcuts 'alphabetic-key
  '( ([?\C-B] project-buffers-menu)
     ([?\M-B] buffer-menu)
     ([?\M-C] clear-buffer)
     
     ([?\M-d]    delete-word-at-point)
     ([?\M-\C-d] open-this-directory)
     ([?\M-D]    dired-default-directory)

     ([?\C-e]    sql-execute-query-at-point)

     ([?\M-F]    font-lock-mode)
     ([?\M-\C-F] show-face-at-point)

     ([?\C-\M-g] goto-line)
     ([?\M-G]    git-gui-this-directory)

     ([?\M-I] filter-on-indent)

     ([?\M-K] kill-buffer-y-or-n)

     ([?\M-L] thin-comment-line)

     ([?\M-M]    build-project)
     ([?\C-\M-M] build-project-with-target)

     ([?\M-n] revert-if-saved)

     ([?\M-o]    filter-on-word-at-point)
     ([?\M-O]    toggle-filtering)

     ([?\M-p] visit-project-file)
     ([?\M-P] show-position)

     ([?\M-Q] indent-sexp)

     ([?\M-S] start-killable-shell)
     ([?\C-\M-S] regenerate-sass-watch)

     ([?\C-t]    toggle-truncate-lines)
     ([?\C-\M-t] toggle-truncate-lines)
     ([?\M-T]    thick-comment-line)

     ([?\C-u] undo)

     ([?\C-w] search-this-directory)

     ([?\M-z] show-messages-buffer)
     ) )

(set-keyboard-shortcuts 'numeric-key
  '( ([?\M-0] delete-other-windows)
     ([?\M-7] delete-window)
     ([?\M-8] window-only-and-split)
     ([?\M-9] split-window-vertically)
     ) )

(set-keyboard-shortcuts 'punctuation-key
  '( ([?\M--] other-window)
     ([?\C-+] cycle-frame-fonts)
     ([?\C-'] start-single-quoted-string)
     ([?\C-\"] start-double-quoted-string)
     ) )

(set-keyboard-shortcuts 'multi-key
  '( ([?\C-x ?k] kill-buffer-y-or-n)
     ) )

(set-keyboard-shortcuts 'fN-key
  '( ([f2] my-expand-abbrev)

     ([f3]   save-buffer)
     ([S-f3] save-and-kill-buffer)
     ([M-f3] save-this-buffer-and-others)

     ([f4]     find-file-at-point)
     ([C-f4]   find-file-at-point-full-window)
     ([S-f4]   visit-parsed-file-location)
     ([M-f4]   run-file-at-point)
     ([S-M-f4] execute-this-file)

     ([f5] save-mouse-selection)

     ([C-f6] start-or-end-kbd-macro)
     ([S-f6] call-last-kbd-macro)

     ([f7]     open-home-file-menu)
     ([C-f7]   edit-emacs-customisation)
     ([C-M-f7] open-project-file-menu)
     ([C-S-f7] visit-sql-file)
     ([M-f7]   open-file-menu)
     ([S-f7]   visit-project-file-menu)
     ([S-M-f7] open-home-file-menu-other-window)

     ([M-f8] run-alternate-command-on-file-or-dir)
     ([S-f8] stop-on-1st-test-failure-minor-mode)
     ([M-S-f8] project-run-alternate-command)
     ([C-S-f8] project-run-url-in-dev-browser)

     ([S-C-f9] kill-buffer-process)
     ([M-f9] project-run-this-file)
     
     ([S-M-f9] project-run-project)

     ([C-f10]   magit-status)
     ([C-S-f10] git-fetch-and-view)
     ([C-M-f10] comment-region)
     ([S-M-f10] gitk-this-directory)

     ([C-f11]   describe-variable-at-pos)
     ([S-f11]   describe-function-at-pos)
     ([S-C-f11] describe-key)

     ([f12]   repeat-complex-command)
     ([C-f12] project-search-for-definition-at-point)
     ([M-f12] project-search-for-identifier-at-point)
     ([S-M-f12] project-search-census)
     ([C-S-f12] project-search-for-identifier-part-at-point)
     ) )

(set-keyboard-shortcuts 'directional-key
 '( ([M-S-down]   point-to-bottom)

    ([home]   beginning-of-line)
    ([C-home] beginning-of-buffer)

    ([end]   end-of-line)
    ([C-end] end-of-buffer)

    ([C-left] scroll-left-a-bit)
    ([C-kp-left] scroll-left-a-bit)
    ([M-left]    backward-sexp)

    ([C-right] scroll-right-a-bit)
    ([C-kp-right] scroll-right-a-bit)
    ([M-right]    forward-sexp)

    ([C-up] scroll-up-one)
    ([C-kp-up] scroll-up-one)
    ([C-S-up] previous-error)
    ([C-S-kp-up] previous-error)
    ([M-up]    find-comment-line-bwd)
    ([M-S-up]  point-to-top)

    ([C-S-down] next-error)
    ([C-S-kp-down] next-error)
    ([C-down] scroll-down-one)
    ([C-kp-down] scroll-down-one)

    ([M-down] find-comment-line-fwd)

    ([M-next] find-thick-comment-line-fwd)

    ([M-prior] find-thick-comment-line-bwd)
    ) )

(set-keyboard-shortcuts 'named-key
  '( ([pause]   next-error)
     ([S-pause] previous-error) 
     
     ([kp-delete] kill-region)
     ([delete] kill-region)
     ([insert] yank)
     ) )

(set-keyboard-shortcuts 'mouse
  '( ([mouse-3]     paste-word)
     ([C-S-mouse-3] paste-line)
     ([M-mouse-3]   paste-filename)
     ([S-mouse-3]   replace-word)
     ) )

;;-----------------------------------------------------------------
;;    ([?\M-\C-S] spanish-minor-mode)
;;    ([f8] spanish-toggle-accent)
;;    ([?\C-\M-l] maori-lookup-word-in-dictionary)
