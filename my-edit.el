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

;-----------------------------------------------------------------
(defun text-hook ()
  (local-unset-key [?\M-S]) )

(setq text-mode-hook '(text-hook))

;;-----------------------------------------------------------------
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

;;-----------------------------------------------------------------
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

;;-----------------------------------------------------------------

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

;; For emacs 24, re-instate auto-copy when selecting region with mouse
(setq mouse-drag-copy-region t)

(set-face-attribute 'region nil :background "#ffff00" :foreground "#000")
