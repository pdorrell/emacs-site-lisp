;; Copyright (C) 2000,2001 Philip Dorrell

;; Last changes (to all files) : 27 Sept 2000

(load "cl")

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)

(setq html-script-toggle-key [C-M-S-pause])

(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

(column-number-mode t)

;-----------------------------------------------------------------
(set-face-background 'region "yellow")
(set-cursor-color "blue")
(setq search-highlight t)
(global-set-key [S-f6] 'call-last-kbd-macro)
(global-set-key [?\M-Q] 'indent-sexp)
;-----------------------------------------------------------------

(setq emacs-customisation-dir (file-name-directory load-file-name))
(setq emacs-util-classpath (concat emacs-customisation-dir "emacs.jar"))

(load "executables")

(load "my-essential")
(load "my-utils")
(load "my-project")
(load "my-movement")
(load "my-edit")
(load "my-process")
(load "my-completion")
(load "my-search")
(load "my-word")
(load "my-faces")
(load "my-abbrev")
(load "my-emacs-lisp")
(load "my-comment-line")
(load "my-file")
(load "my-shell")
(load "my-prog")
(load "my-java")
(load "my-android")
(load "my-html")
(load "my-window")
(load "my-tomcat")
(load "my-browser")
(load "my-sql")
(load "file-menu")
(load "my-python")
(load "my-spanish")
(load "my-latex")
;;(load "my-postscript")
(load "my-lisp")
;;(load "my-plisp")
(load "my-ruby")
(load "my-haxe")
;;(load "my-django")
;;(load "my-drools")
(load "my-gae")

(load "nxml-mode-20041004/rng-auto.el")

(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
	    auto-mode-alist))

(autoload 'spanish-minor-mode "my-spanish")

(setq running-windows (fboundp 'w32-using-nt))

(setq running-windows-9x (and (fboundp 'w32-using-nt) (not (w32-using-nt))))

(if (boundp 'w32-fixed-font-alist)	
	(set-default-font "Fixedsys") )

(setq transient-mark-mode nil)

(defun yes-or-no-p (string)
  (y-or-n-p string) )

(global-unset-key [f2])
(global-set-key [f2] 'my-expand-abbrev)
