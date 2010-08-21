(set-extension-mode ".apk" 'archive-mode)

(defvar *android-sdk-directory* nil "Location of Android SDK")

(defun android-run-avd (name)
  "Run Android Virtual Device with NAME in the Android emulator"
  (start-process "*android-emulator*" nil 
		 (concat *android-sdk-directory* "tools/emulator")
		 "-avd" name) )

