;; Copyright (C) 2001 Philip Dorrell

(setq tomcat-home-dir (getenv "TOMCAT_HOME"))

(defvar tomcat-script (concat tomcat-home-dir "\\bin\\tomcat.bat") "Script to start or stop tomcat")

(defvar tomcat-process nil "Process running tomcat")
(defvar tomcat-buffer nil "Buffer containing tomcat-process")
(defvar tomcat-is-running nil "Is tomcat running ?")

(defun tomcat-start ()
  (switch-to-buffer "*tomcat*")
  (setq tomcat-buffer (current-buffer))
  (clear-buffer)
  (setq tomcat-process (start-process "tomcat" (current-buffer) tomcat-script "run"))
  (setq tomcat-is-running t)
  (message "Tomcat started") )

(defun tomcat-stop ()
  (message "Stopping Tomcat ...")
  (save-excursion
    (switch-to-buffer "*tomcat*")
    (goto-char (point-max)) )
  (call-process tomcat-script nil "*tomcat-stop*" t "stop")
  (setq tomcat-is-running nil)
  (message "Tomcat stopped") )

(defun tomcat-toggle()
  "Stop or start Tomcat"
  (interactive)
  (if tomcat-is-running
      (tomcat-stop)
    (tomcat-start) ) )

(global-set-key [?\C-\M-t] 'tomcat-toggle)
