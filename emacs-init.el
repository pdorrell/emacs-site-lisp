
(defmacro try-to (command)
  "Try to run COMMAND, if it fails, skip, or give option of re-running without trapping error"
  `(condition-case exception
       ,command
     (error (if 
                (y-or-n-p (format "Trapped error running %s, skip? " 
                                  ',command))
                (message "Skipped exception %s attempting to run %s" exception ',command)
              ,command)) ) )

(defmacro try-to-load (file)
  `(try-to (load ',file)) )

(setq gc-cons-threshold 20000000)

(setq print-length 20)
(setq print-level 6)
(setq eval-expression-print-length 20)
(setq eval-expression-print-level 6)
(setq print-circle t)

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-splash-screen t)

(setq x-select-enable-clipboard t)

(setq mouse-1-click-follows-link nil)

(try-to-load "my-testing")

(running-tests "Loading my-start.el"
               (try-to-load "my-start.el") )

(setq sql-file "~/test.sql")

(open-home-file-menu)

(setq spell-command "aspell")

(setq ispell-dictionary "british")

(setq archive-zip-use-pkzip nil)

(setq dired-dwim-target t)

(setenv "PS1" "\\w> ")

(setq split-height-threshold 40)
(setq split-width-threshold 1000)

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

