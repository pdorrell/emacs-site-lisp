(require 'cl-extra)

(setq *language-by-extension* (make-hash-table :test 'equal))

(defun set-language-for-extension(extension language)
  (puthash extension language *language-by-extension*) )

(set-language-for-extension "py" 'python)
(set-language-for-extension "el" 'emacs-lisp)
(set-language-for-extension "rb" 'ruby)

(defun get-current-language()
  (let* ( (extension (file-name-extension (buffer-file-name)))
          (current-language (gethash extension *language-by-extension*)) )
    (if (not current-language)
        (error "No language found for extension %s" extension) )
    current-language) )

(defun get-language-value-for-project (language value-key)
  (let* ( (value-key-values (project-value value-key))
          (language-value (if value-key-values (cdr (assoc language value-key-values))) ))
    language-value) )

(defun get-project-name-from-base-dir()
  (let ( (base-dir (project-base-directory-value)) )
    (file-name-nondirectory (directory-file-name base-dir)) ) )

(defun get-project-name()
  (let ( (project-name (project-value :project-name)) )
    (or project-name (get-project-name-from-base-dir)) ) )

(setq *default-executable-load-path* (concat emacs-customisation-dir "/bin/"))

(setq *run-command-in-directory-script* 
      (concat *default-executable-load-path* "run-command-in-directory"))

(defun get-project-executable-load-path()
    (list (concat (project-base-directory-value) "_project/bin/")
          *default-executable-load-path*) )

(defun file-if-it-exists (file-path)
  (if (file-exists-p file-path)
      file-path) )

(defun get-project-executable (executable-name)
  (let* ( (load-path (get-project-executable-load-path))
          (executable-path (cl-some 
                            (lambda (path) (file-if-it-exists (concat path executable-name)))
                            load-path) ) )
    (if (not executable-path)
        (error "Executable %S not found on load path %S" executable-name load-path) )
    executable-path) )

;; test/test-project/src/subdir/hello_world.py

(defun script-to-other-window (script-path working-dir output-buffer-name command-args)
  (apply 'start-process-showing-console 
         output-buffer-name output-buffer-name 
         *run-command-in-directory-script* working-dir
         script-path command-args) )

(defun run-project-command (run-script-fun working-dir-getter script command-args-getter)
  (let* ( (output-buffer-name (concat "*" (get-project-name) "-" script "*"))
          (resolved-script-path (get-project-executable script))
          (working-dir (funcall working-dir-getter))
          (command-arg-or-args (funcall command-args-getter))
          (command-args (if (listp command-arg-or-args) command-arg-or-args (list command-arg-or-args))) )
    (funcall run-script-fun resolved-script-path working-dir output-buffer-name command-args) ) )

(defun project-run-this-file()
  (interactive)
  (let* ( (current-language (get-current-language))
          (run-this-file-command (get-language-value-for-project current-language :run-this-file)) )
    (if run-this-file-command
        (apply 'run-project-command run-this-file-command)
      (run-this-file) ) ) )

(global-set-key [M-f9] 'project-run-this-file)
