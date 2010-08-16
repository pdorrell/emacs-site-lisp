;; Copyright (C) 2000 Philip Dorrell

;(list-faces-display)
;(list-colors-display)

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

(copy-face 'font-lock-function-name-face 'html-tag-face)

(copy-face 'html-tag-face 'ftl-tag-face)
(set-face-background 'ftl-tag-face "LightSkyBlue1")
(setq ftl-tag-face 'ftl-tag-face)
(copy-face 'html-tag-face 'ftl-macro-tag-face)
(set-face-background 'ftl-macro-tag-face "khaki1")
(setq ftl-macro-tag-face 'ftl-macro-tag-face)

(copy-face 'html-tag-face 'ftl-expr-face)
(setq ftl-expr-face 'ftl-expr-face)
(set-face-background 'ftl-expr-face "misty rose")

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

(global-set-key [?\M-F] 'font-lock-mode)
(global-set-key [?\M-\C-F] 'show-face-at-point)

(defun unbold-all-faces ()
  "Clear the `bold' flag from all faces."
  (interactive)
  (dolist (f (face-list))
    (if (face-bold-p f) (set-face-bold-p f nil))))

(unbold-all-faces)

(add-hook 'font-lock-mode-hook
          (lambda ()
            (unbold-all-faces)))

