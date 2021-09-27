(defvar file-menu-file-name "_" "Name used for file menus")

;-----------------------------------------------------------------

(defvar *base-file-menu*
  "~/_" "The base file menu")

(defun messaged-find-file (filename)
  "Find FILENAME, and message its name."
  (find-file filename)
  (message "%s" (buffer-file-name)) )

(defun open-base-file-menu ()
  "Open 'home' file menu"
  (interactive)
  (messaged-find-file *base-file-menu*) )

(defun open-file-menu ()
  "Open file menu"
  (interactive)
  (messaged-find-file file-menu-file-name) )

(defun open-file-menu-other-window ()
  "Open file file menu in other window"
  (interactive)
  (find-file-other-window file-menu-file-name) 
  (message "%s" (buffer-file-name)) )

(defun open-base-file-menu-other-window ()
  "Open file file-menu in other window"
  (interactive)
  (find-file-other-window *base-file-menu*) 
  (message "%s" (buffer-file-name)) )
