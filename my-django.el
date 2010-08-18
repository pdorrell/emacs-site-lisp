
(defvar *django-server-process* nil "Django server process")
(defvar *django-server* nil "Python script to start django server")

(defun django-run-server()
  (interactive)
  (if (buffer-for-name "*django*")
      (kill-buffer "*django*") )
  (setf *django-server-process*
	(start-process "django-process" "*django*" *python-executable* "-u" "-v"
		       *django-server* "runserver") )
  (switch-to-buffer-other-window "*django*") )


(defun stop-start-django-server()
  (interactive)
  (stop-start-process "Django process" '*django-server-process* "*django*"
		      *python-executable* "-u"
		      django-application-start-file "runserver") )

(defun show-django-output()
  (interactive)
  (switch-to-buffer "*django*")
  (goto-char (point-max))
  (if (search-backward-regexp (elt python-line-matcher 0) nil t)
    (progn
      (delete-other-windows)
      (split-window-vertically)
      (visit-file-line) )
    (progn
      (message "No errors in django output") ) ) )

(def-html-abbrev "if" '("{% if " mark " %}" return return "{% endif %}" return goto-mark))
(def-html-abbrev "e" '("{% else %}" return))
(def-html-abbrev "ei" '("{% elseif %}" return))
(def-html-pair-abbrev "v" "{{" "}}")
(def-html-abbrev "for" '("{% for " mark " in  %}" return "{% endfor %}" return goto-mark))

(def-html-pair-abbrev "block" "{% block " " %}{% endblock %}")
(def-html-pair-abbrev "extends" "{% extends \"" "\" %}")