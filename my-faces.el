;; Copyright (C) 2000 Philip Dorrell

;;(list-faces-display)
;(list-colors-display)

;; Default colour theme that I prefer

(font-lock-mode -1)

(set-face-foreground font-lock-keyword-face "medium blue")

(set-face-foreground font-lock-function-name-face "black")
(set-face-background font-lock-function-name-face "DarkOliveGreen1")

(set-face-foreground font-lock-comment-face "black")
(set-face-background font-lock-comment-face "light goldenrod")

(set-face-foreground font-lock-string-face "dark red")

(set-face-foreground font-lock-type-face "black")

(set-face-foreground font-lock-variable-name-face "black")

(set-face-foreground font-lock-warning-face "black")
(set-face-background font-lock-warning-face "LemonChiffon2")

(copy-face 'font-lock-function-name-face 'web-mode-html-tag-face)
(copy-face 'font-lock-keyword-face 'web-mode-html-attr-name-face)
(copy-face 'font-lock-function-name-face 'web-mode-html-tag-bracket-face)


(defmacro def-bg-face (name color)
  `(progn
     (setq ,name (make-face ',name))
     (set-face-foreground ',name "black")
     (set-face-background ',name ,color) ) )

;; This is some commentary

(defun show-face-at-point ()
  "Show name of face at point"
  (interactive)
  (message "%s" (get-text-property (point) 'face)) )

(defun deliberate-error ()
  "Dummy function"
  (interactive)
  (error "You shouldn't call this function") )

(defun unbold-all-faces ()
  "Clear the `bold' flag from all faces"
  (interactive)
  (dolist (f (face-list))
    (if (face-bold-p f) (set-face-bold-p f nil))))

(unbold-all-faces)

;; Unbold all the faces, because bold doesn't look very good with my preferred font
(add-hook 'font-lock-mode-hook
          (lambda ()
            (unbold-all-faces)))
