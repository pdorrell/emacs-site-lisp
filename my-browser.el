
(defvar browse-url-startup-command
  (list *java-executable* "-cp" emacs-util-classpath "com1729.emacs.browser.Browser") )

(defun browse-url-in-buffer (url &optional base-url)
  (switch-to-buffer "*browse-url*")
  (let ( (browse-lines
	  (if base-url (list "3" "browseRelativeUrl" base-url url)
	    (list "2" "browseUrl" url) ) ) )
    (run-process-with-lines "browse_url" browse-url-startup-command browse-lines nil t)
    (browse-url-mode url) ) )

(defvar browse-url-mode-map nil)
(progn
  (setq browse-url-mode-map (make-sparse-keymap))
  (define-key browse-url-mode-map [f4] 'browse-url-at-point-in-buffer)
  (define-key browse-url-mode-map [?\M-K] 'bury-buffer)
  (define-key browse-url-mode-map [?\M-\C-K] 'kill-buffer-y-or-n)
  (define-key browse-url-mode-map [M-left] 'browse-previous-url-in-buffer)
  (define-key browse-url-mode-map [M-kp-left] 'browse-previous-url-in-buffer)
  (define-key browse-url-mode-map [?\M-R] 'browser-url-in-browser)
  (define-key browse-url-mode-map [?\M-r] 'browser-reload-url-in-buffer)
  )

(setq url-word-table
      (make-alpha-table letters-digits-string
			"`~!@#$%^&*()-_=+[{]};:'|\\,<.>/?") )

(defun get-current-url()
  (let (url)
    (setq url (substring (buffer-line (point-min)) 6))
    url) )

(defun browser-url-in-browser()
  (interactive)
  (browse-url (get-current-url)) )

(defun browser-reload-url-in-buffer()
  (interactive)
  (browse-url-in-buffer (get-current-url)) )

(defun browse-previous-url-in-buffer ()
  (interactive)
  (if url-history
      (let ( (previous-url (car url-history)) )
	(setq url-history (cdr url-history))
	(browse-url-in-buffer previous-url) )
    (message "No previous URL") ) )


(defun browse-url-at-point-in-buffer ()
  (interactive)
  (let ( (url (word-at url-word-table (point)))
	 (current-url (get-current-url)) )
    (setq url-history (cons current-url url-history))
    (browse-url-in-buffer url current-url) ) )

(defun browse-url-mode (url)
  (setq major-mode 'browse-url-mode
	mode-name "Browse URL" )
  (use-local-map browse-url-mode-map)
  (make-local-variable 'url-history)
  (if (not (boundp 'url-history)) (setq url-history nil))
  (make-local-variable 'current-browser-url)
  (setq current-browser-url url)
  (font-lock-mode 0)
  (font-lock-mode 1) )

;;   ***http://localhost/
