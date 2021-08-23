;; Copyright (C) 2000-2016 Philip Dorrell

(setq kmacro-call-mouse-event nil) ;; to stop it hijacking S-mouse-3

(setq running-windows-9x (and (fboundp 'w32-using-nt) (not (w32-using-nt))))

(if (boundp 'w32-fixed-font-alist)	
	(set-frame-font "Consolas-11")
  (set-frame-font "Liberation Mono-13") )

(setq-default indent-tabs-mode nil)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(setq emacs-customisation-dir "/home/philip/dev/emacs/emacs-site-lisp")
(setq emacs-util-classpath (concat emacs-customisation-dir "/emacs.jar"))
(setq package-user-dir (concat emacs-customisation-dir "/elpa"))

(require 'package)
(package-initialize)

(setq running-windows (fboundp 'w32-using-nt))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(try-to-load "cl")

(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

(column-number-mode t)

(setq indent-tabs-mode nil)

;-----------------------------------------------------------------
(set-face-background 'region "yellow")
(set-cursor-color "blue")
(setq search-highlight t)

;-----------------------------------------------------------------

(try-to-load "executables")

(try-to-load "my-essential")
(try-to-load "my-utils")
(try-to-load "my-modes")
(try-to-load "my-global-keyboard-shortcuts")

(try-to-load "my-project")
(try-to-load "my-movement")
(try-to-load "my-edit")
(try-to-load "my-line-filtering")
(try-to-load "my-process")
(try-to-load "my-completion")
(try-to-load "my-search")
(try-to-load "my-word")
(try-to-load "my-faces")
(try-to-load "my-abbrev")
(try-to-load "my-file-menu")
(try-to-load "my-emacs-lisp")
(try-to-load "my-comment-line")
(try-to-load "my-file")
(try-to-load "my-line-matchers")
(try-to-load "my-shell")
(try-to-load "my-prog")
(try-to-load "my-java")
(try-to-load "my-javascript")
(try-to-load "my-html")
(try-to-load "my-window")
(try-to-load "my-browser")
(try-to-load "my-sql")
(try-to-load "file-menu")
(try-to-load "my-python")
(try-to-load "my-spanish")
(try-to-load "my-lisp")
(try-to-load "my-ruby")
(try-to-load "my-linux")
(try-to-load "my-sass")
(try-to-load "my-maori")
(try-to-load "my-idris")

(try-to-load "my-melody-scripter")

(autoload 'spanish-minor-mode "my-spanish")

(setq transient-mark-mode nil)

(defun yes-or-no-p (string)
  (y-or-n-p string) )

(setq enable-local-variables :safe)

;; fix-up
(global-set-key [S-mouse-3] 'replace-word)
