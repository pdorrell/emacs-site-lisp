
(setq gc-cons-threshold 20000000)

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-splash-screen t)

(setq mouse-1-click-follows-link nil)

(defvar *classpath-separator* ";")

(defvar *my-name* "<Put your name here>" "My name")

(defvar copyright-line (concat "Copyright (C) 2010 " *my-name*))

(global-set-key [?\M-\C-S] 'spanish-minor-mode)

(load "my-start.el")

(setq sql-file "~/test.sql")

(def-abbrev-fun java-before-package()
  (insert "// " copyright-line "\n\n") )

(def-abbrev-fun java-after-package()
  (insert "\n/**\n" " * \n" " * @author " *my-name* "\n" " */\n") )

(open-base-file-menu)

(setq vc-cvs-stay-local nil)

(setq spell-command "aspell")

(setq ispell-dictionary "british")

(setq archive-zip-use-pkzip nil)

(setq dired-dwim-target t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(bold ((t (:weight normal)))))

