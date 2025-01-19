;; Copyright (C) 2000,2001 Philip Dorrell

;-----------------------------------------------------------------
(defun text-hook ()
  "Hook function for text mode"
  (local-unset-key [?\M-S])  ;; remove M-S defn for text mode to unmask global M-S (start-killable-shell)
  (setq run-file-function #'open-file-in-firefox)
  )

(setq text-mode-hook '(text-hook))

;;-----------------------------------------------------------------
(defun start-or-end-kbd-macro ()
  "If defining a keyboard macro, finish it, else start one"
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil) ) )

;;-----------------------------------------------------------------
(defun clear-buffer ()
  "Clear the buffer as long as the buffer is not visiting file."
  (interactive)
  (if (buffer-file-name)
      (message "Cannot clear a buffer visiting a file")
    (progn
        (delete-region (point-min) (point-max))
        (if (memq major-mode '(shell-mode rlogin-mode))
            (comint-send-input) ) ) ) )

;;-----------------------------------------------------------------
(defun revert-if-saved()
  "Revert the buffer to the file on disk, but only if there are no unsaved changes."
  (interactive)
  (if (buffer-modified-p)
      (message "Buffer has been modified, save it first")
    (progn
      (revert-buffer t t)
      (message "Buffer reloaded from current file contents.") ) ) )

(defun shift-initial-case ()
  "Toggle the case of the first character in the word at point."
  (interactive)
  (let* ( (pos (point))
          (word (word-before word-alpha-table pos))
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

;; Show 'region' selected by mouse as black text on yellow background.
(set-face-attribute 'region nil :background "#ffff00" :foreground "#000")

;;-----------------------------------------------------------------
(defun trim-trailing-whitespace-in-buffer()
  "If *trim-trailing-whitespace-on-save* is set for the buffer, delete all trailing whitespace
   (ie from ends of lines or end of file)"
  (when *trim-trailing-whitespace-on-save*
    (message "trim-trailing-whitespace-in-buffer ...")
    (delete-trailing-whitespace (point-min) nil) ) )

(defvar *trim-trailing-whitespace-on-save* nil "Should trailing whitespace be trimmed on save?")
(make-variable-buffer-local '*trim-trailing-whitespace-on-save*)

(add-hook 'before-save-hook 'trim-trailing-whitespace-in-buffer)

(setq *large-frame-font* nil)

(defun toggle-large-frame-font()
  "Toggle between large and small frame font"
  (interactive)
  (setq *large-frame-font* (not *large-frame-font*))
  (set-frame-font (if *large-frame-font* *large-font-name* *small-font-name*)) )
