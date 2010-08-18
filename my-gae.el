(defvar *gae-python-executable* nil "Python executable for running GAE")
(defvar *gae-server-process* nil "GAE server process")
(defvar *gae-application-datastore* nil "Datastore for GAE application")
(defvar *gae-appengine-server* nil "Python script to run GAE server")

(defun stop-start-gae-server()
  (interactive)
  (setq *show-project-log-buffer-function* 'show-gae-output)
  (stop-start-process "GAE process" '*gae-server-process* "*GAE*"
		      (project-file :python-executable) (project-file :gae-server)
		      (concat "--datastore_path=" (project-file :gae-datastore))
		      "--admin_console_server" "--port=8080" (project-file :gae-app-directory)) )

(defun show-gae-output()
  (interactive)
  (switch-to-buffer "*GAE*")
  (goto-char (point-max))
  (if (search-backward-regexp (elt python-line-matcher 0) nil t)
    (progn
      (delete-other-windows)
      (split-window-vertically)
      (visit-file-line) )
    (progn
      (message "No errors in gae output") ) ) )

