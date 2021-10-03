(defun sudo-gedit ()
  "Synchronous function to gedit current file using sudo"
  (interactive)
  (save-buffer)
  (start-process "*gedit-admin*" nil "/usr/bin/gedit" (concat "admin://" (buffer-file-name)))
;;  (call-process "/usr/bin/xterm" nil "*sudo-buffer*" nil "-e" "/usr/bin/gksu" "-k" "/usr/bin/gedit" (buffer-file-name))
  (revert-if-saved) )

(defun gedit ()
  "Synchronous function to gedit current file"
  (interactive)
  (save-buffer)
  (start-process "*gedit*" nil "/usr/bin/gedit" (buffer-file-name)) )
