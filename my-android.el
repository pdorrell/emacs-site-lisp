(set-extension-mode ".apk" 'archive-mode)

(defvar *android-sdk-directory* nil "Location of Android SDK")

(defun android-run-avd (name)
  "Run Android Virtual Device with NAME in the Android emulator"
  (setq *show-project-log-buffer-function* 'android-view-logs)
  (start-process "*android-emulator*" nil 
		 (concat *android-sdk-directory* "tools/emulator")
		 "-avd" name) )

(defvar *android-log-buffer* nil "Buffer containing Android log output")

(defun android-view-logs ()
  "View Android logs in a buffer via adb logcat"
  (interactive)
  (when (not (and *android-log-buffer* (buffer-live-p *android-log-buffer*)))
    (setq *android-log-buffer* (get-buffer-create "*android-log*"))
    (set-process-query-on-exit-flag
     (start-process "*android-logs*" *android-log-buffer*
		    (concat *android-sdk-directory* "tools/adb.exe") "logcat")
     nil) )
  (switch-to-buffer *android-log-buffer*)
  (toggle-truncate-lines 1)
  (goto-char (point-max)) )
       
(defun android-run-java-file ()
  "Android version of java-run-main, which also shows android log buffer, so you can see std out"
  (interactive)
  (java-run-main)
  (other-window 1)
  (android-view-logs) )
