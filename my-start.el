;; Copyright (C) 2000-2016 Philip Dorrell

(setq kmacro-call-mouse-event nil) ;; to stop it hijacking S-mouse-3

(setq running-windows-9x (and (fboundp 'w32-using-nt) (not (w32-using-nt))))

(if (boundp 'w32-fixed-font-alist)	
	(set-frame-font "Consolas-11")
  (set-frame-font "Liberation Mono-13") )

(setq-default indent-tabs-mode nil)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(require 'package)
;;(package-initialize)

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
(global-set-key [S-f6] 'call-last-kbd-macro)
(global-set-key [?\M-Q] 'indent-sexp)
;-----------------------------------------------------------------

(try-to-load "executables")

(try-to-load "my-essential")
(try-to-load "my-utils")
(try-to-load "my-project")
(try-to-load "my-movement")
(try-to-load "my-edit")
(try-to-load "my-process")
(try-to-load "my-completion")
(try-to-load "my-search")
(try-to-load "my-word")
(try-to-load "my-faces")
(try-to-load "my-abbrev")
(try-to-load "my-emacs-lisp")
(try-to-load "my-comment-line")
(try-to-load "my-file")
(try-to-load "my-shell")
(try-to-load "my-prog")
(try-to-load "my-java")
(try-to-load "my-javascript")
;;(try-to-load "my-android")
(try-to-load "my-html")
(try-to-load "my-window")
;;(try-to-load "my-tomcat")
(try-to-load "my-browser")
(try-to-load "my-sql")
(try-to-load "file-menu")
(try-to-load "my-python")
(try-to-load "my-spanish")
;;(try-to-load "my-latex")
;;(try-to-load "my-postscript")
(try-to-load "my-lisp")
;;(try-to-load "my-plisp")
(try-to-load "my-ruby")
;;(try-to-load "my-haxe")
;;(try-to-load "my-django")
;;(try-to-load "my-drools")
;;(try-to-load "my-gae")
(try-to-load "my-net")
(try-to-load "my-linux")
;;(try-to-load "my-treetop")
(try-to-load "my-sass")
(try-to-load "my-maori")
;;(try-to-load "my-coffee")
(try-to-load "my-rst")
(try-to-load "my-idris")

(try-to-load "my-melody-scripter")

(autoload 'spanish-minor-mode "my-spanish")

(setq transient-mark-mode nil)

(defun yes-or-no-p (string)
  (y-or-n-p string) )

(global-unset-key [f2])
(global-set-key [f2] 'my-expand-abbrev)

(setq enable-local-variables :safe)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Mouse-3 fix-ups

(global-set-key [S-mouse-3] 'replace-word)

