(defvar file-menu-file-name "_" "Name used for file menus")

;-----------------------------------------------------------------

(defvar *home-file-menu*
  "~/_" "The base file menu")

(defun messaged-find-file (filename)
  "Find FILENAME, and message its name."
  (find-file filename)
  (message "%s" (buffer-file-name)) )

(defun open-home-file-menu ()
  "Open 'home' file menu"
  (interactive)
  (messaged-find-file *home-file-menu*) )

(defun open-file-menu ()
  "Open file menu"
  (interactive)
  (messaged-find-file file-menu-file-name) )

(defun open-file-menu-other-window ()
  "Open file file menu in other window"
  (interactive)
  (find-file-other-window file-menu-file-name) 
  (message "%s" (buffer-file-name)) )

(defun open-home-file-menu-other-window ()
  "Open file file-menu in other window"
  (interactive)
  (find-file-other-window *home-file-menu*) 
  (message "%s" (buffer-file-name)) )
