(require 'cl-extra)

(defun project-value-for-language(value-key language)
  (project-value (cons value-key language)) )

(defun get-project-command (value-key &optional language)
  (if language
      (project-value-for-language value-key language)
    (project-value value-key) ) )

(defun get-project-name-from-base-dir()
  (let ( (base-dir (get-current-project-base-directory)) )
    (file-name-nondirectory (directory-file-name base-dir)) ) )

(defun get-project-name()
  (let ( (project-name (project-value :project-name)) )
    (or project-name (get-project-name-from-base-dir)) ) )

(setq *default-executable-load-path* (concat emacs-customisation-dir "/bin/"))

(setq *run-command-in-directory-script* 
      (concat *default-executable-load-path* "run-command-in-directory"))

(defun get-project-executable-load-path()
    (list (concat (get-current-project-base-directory) "_project/bin/")
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

(defun script-to-other-window (script-path working-dir output-buffer-name command-args &optional move-to-top)
  (let ( (process-variable-symbol (intern (concat "process-runner-process-" output-buffer-name))) )
    (if (not (boundp process-variable-symbol))
        (set process-variable-symbol nil) )
    (funcall 'stop-then-start-process 
             output-buffer-name process-variable-symbol output-buffer-name
             *run-command-in-directory-script*
             (append (list working-dir script-path)
                     command-args)
             move-to-top)
    (with-current-buffer output-buffer-name
      (toggle-truncate-lines t) ) ) )

(defun script-to-other-window-show-top (script-path working-dir output-buffer-name command-args)
  (script-to-other-window script-path working-dir output-buffer-name command-args t) )

(defun set-total-window-height (window height)
  (let ( (delta (- height (window-total-height window))) )
    (window-resize window delta) ) )

(defun sync-script-to-other-short-window (script-path working-dir output-buffer-name command-args)
  (let ( (output-buffer (get-buffer-create output-buffer-name)) )
    (message "Running %s on %S" script-path command-args)
    (display-buffer output-buffer)
    (save-selected-window
      (set-buffer output-buffer)
      (clear-buffer)
      (apply 'call-process script-path nil '(t t) 't command-args)
      (message "   finished running %s on %S" script-path command-args)
      (set-window-point (get-buffer-window output-buffer) (point-max))
      (set-total-window-height (get-buffer-window output-buffer) 6) )
    (revert-if-saved) ) )

(dolist (fun-type '(run-script-fun working-dir-getter command-args-getter output-buffer-dir-getter))
  (put '*run-project-funs* fun-type (make-hash-table :test 'eq)) )

(defun def-run-project-fun (fun-type key fun)
  (puthash key fun (get '*run-project-funs* fun-type)) )

(defun get-run-project-fun (fun-type key)
  (let ( (fun (gethash key (get '*run-project-funs* fun-type))) )
    (if (not fun)
        (error "Cannot get run-project function of type %s and key %s" fun-type key) )
    fun ) )

(defun get-this-file-or-directory-name()
  (let ( (buffer-file-name (buffer-file-name)) )
    (if (not buffer-file-name)
	(setq buffer-file-name default-directory) )
    (expand-file-name (buffer-file-name)) ) )

(defun get-no-command-params()
  nil)

(defun get-main-file-output-buffer-dir()
  (let ( (main-file-output-buffer-dir-env-var (project-value :main-file-output-buffer-dir-env-var)) )
    (if main-file-output-buffer-dir-env-var
        (getenv main-file-output-buffer-dir-env-var) ) ) )

(defun get-alternate-command-dir()
  (project-file :alternate-command-dir) )

(def-run-project-fun 'run-script-fun 'other-window 'script-to-other-window)
(def-run-project-fun 'run-script-fun 'other-window-show-top 'script-to-other-window-show-top)
(def-run-project-fun 'run-script-fun 'other-short-window-sync 'sync-script-to-other-short-window)

(def-run-project-fun 'working-dir-getter 'base-dir 'get-current-project-base-directory)
(def-run-project-fun 'working-dir-getter 'alternate-command-dir 'get-alternate-command-dir)

(def-run-project-fun 'command-args-getter 'this-file 'buffer-file-name)
(def-run-project-fun 'command-args-getter 'this-file-or-dir 'get-this-file-or-directory-name)
(def-run-project-fun 'command-args-getter 'main-file 'get-project-main-file)
(def-run-project-fun 'command-args-getter 'nil 'get-no-command-params)

(def-run-project-fun 'output-buffer-dir-getter 'main-file-output-dir 'get-main-file-output-buffer-dir)

(defun get-script-and-args (command-script)
  (if (stringp command-script)
      (list command-script)
    (if (listp command-script) 
        (if (eq (car command-script) :wrapped)
            (let* ( (command-script-args (cdr command-script))
                    (command-script-language (car command-script-args))
                    (script-args (cdr command-script-args))
                    (script (project-required-value (cons :wrapper command-script-language))) )
              (cons script script-args) )
          (error "Unknown command-script type %S" (car command-script)) )
      (error "Command-script must be string of list: %S" command-script) ) ) )

(defun last-file-path-part (file-or-dir-name)
  (if (string-ends-with file-or-dir-name "/")
      (file-name-nondirectory (directory-file-name file-or-dir-name))
    (file-name-nondirectory file-or-dir-name) ) )

(defun get-args-description (args-description-spec command-args)
  (if (stringp args-description-spec)
      args-description-spec
    (if (null args-description-spec)
        nil
      (if (eq args-description-spec 'file)
          (let ( (file-name (last-file-path-part (car (last command-args)))) )
            file-name)
        (error "Unknown args description spec %s" args-description-spec) ) ) ) )

(defun get-script-description (description-spec script command-args)
  (if description-spec
      (let* ( (script-description (car description-spec))
              (args-description-spec (cdr description-spec))
              (args-description (get-args-description args-description-spec command-args)) )
        (if args-description
            (concat script-description "-" args-description)
          script-description) )
    script) )

(defun run-project-command (run-script-fun-key working-dir-getter-key command-script command-args-getter-key
                                               &optional description-spec output-buffer-dir-getter-key)
  (save-this-buffer-and-others)
  (let* ( (run-script-fun (get-run-project-fun 'run-script-fun run-script-fun-key))
          (working-dir-getter (get-run-project-fun 'working-dir-getter working-dir-getter-key))
          (command-args-getter (get-run-project-fun 'command-args-getter command-args-getter-key))
          (script-and-args (get-script-and-args command-script))
          (script (car script-and-args))
          (script-args (cdr script-and-args))
          (resolved-script-path (get-project-executable script))
          (working-dir (funcall working-dir-getter))
          (command-arg-or-args (funcall command-args-getter))
          (command-args (if (listp command-arg-or-args) command-arg-or-args (list command-arg-or-args))) 
          (script-description (get-script-description description-spec script command-args))
          (output-buffer-name (concat "*" (get-project-name) "-" script-description "*"))
          (output-buffer-dir (if output-buffer-dir-getter-key
                                 (funcall (get-run-project-fun 'output-buffer-dir-getter output-buffer-dir-getter-key))
                               nil) ) )
    (funcall run-script-fun resolved-script-path working-dir output-buffer-name (append script-args command-args))
    (with-current-buffer output-buffer-name
      (setq-local default-directory (or output-buffer-dir working-dir)) ) ) )

(defun project-run-this-file()
  "Run the current file"
  (interactive)
  (let* ( (run-this-file-command (get-project-command :run-this-file programming-language)) )
    (if run-this-file-command
        (apply 'run-project-command run-this-file-command)
      (message "No project command for %S" (cons :run-this-file programming-language)) ) ) )

;; test/test-project/src/subdir/hello_world.py

(defun get-project-main-file()
  (project-file :main-file) )

(defun project-run-project()
  "Run the project's main file"
  (interactive)
  (let* ( (run-main-file-command (project-value :run-main-file)) )
    (if run-main-file-command
        (apply 'run-project-command run-main-file-command)
      (message "No :run-main-file command defined in project") ) ) )

(defun run-alternate-command-on-file-or-dir()
  (interactive)
  (let ( (alternate-command (get-project-command :alternate-file-or-dir-command)) )
    (if alternate-command
        (apply 'run-project-command alternate-command)
      (run-alternate-command) ) ) )

(defun project-run-alternate-command()
  "Run an alternate project command"
  (interactive)
  (let* ( (run-alternate-command (project-required-value :run-alternate-command)) )
    (apply 'run-project-command run-alternate-command) ) )
