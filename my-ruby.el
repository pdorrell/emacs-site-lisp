(autoload 'ruby-mode "ruby-mode")
(set-extension-mode ".rb" 'ruby-mode)

(defvar *ruby-executable* *ruby-1.9-executable* "Ruby executable for project")

(defvar *webrick-process* nil)

(set-file-name-mode "rakefile" 'ruby-mode)
(set-file-name-mode "Rakefile" 'ruby-mode)

(setq ruby-word-table
      (make-alpha-table letters-digits-string "_") )

(defun webrick ()
  (interactive)
  (if (buffer-for-name "*webrick*")
      (kill-buffer "*webrick*") )
  (setf *webrick-process*
	(start-process "webrick-process" "*webrick*"  (project-file :ruby-executable *ruby-executable*)
		       (concat (project-base-directory-value) "script/server") "-b" "127.0.0.1") )
  (switch-to-buffer-other-window "*webrick*") )

;;(global-set-key [?\C-\M-k] 'webrick)


(defun insert-hash-key ()
  (interactive)
  (insert " => ") )

(defun ruby-run-file2 (file &rest args)
  (let ( (filename (windowize-filename (expand-file-name file))) )
    (switch-to-buffer-other-window "*ruby*")
    (clear-buffer)
    (let ( (ruby-executable (project-file :ruby-executable *ruby-executable*)) 
	   (ruby-args (project-value :ruby-args nil)) )
      (message "%s %s %s ..." ruby-executable ruby-args filename)
      (apply #'start-process 
	     `("ruby" "*ruby*" ,ruby-executable ,@ruby-args ,filename ,@args) ) ) ) )

(defvar *ruby-process* nil "Running ruby program")

(defun ruby-run-file (file &rest args)
  (let ( (filename (windowize-filename (expand-file-name file))) 
	 (current-directory default-directory) )
    (let ( (ruby-executable (project-file :ruby-executable *ruby-executable*)) 
	   (ruby-args (project-value :ruby-args nil)) )
    (switch-to-buffer-other-window "*ruby*")
    (setq default-directory current-directory)
    (clear-buffer)
    (stop-then-start-process "ruby" '*ruby-process* "*ruby*" 
			     ruby-executable (append ruby-args (list filename) args) ) ) ) )

(defun ruby-visit-output-buffer()
  (interactive)
  (switch-to-buffer "*ruby*") )

(defun ruby-insert-member-equals ()
  "Do @x=x on preceding x"
  (interactive)
  (insert-tranformed-word 
   ruby-word-table 
   (lambda (var) (concat "@" var " = " var) )
   "variable name") )

(defun ruby-insert-print-this ()
  "Do puts \"x=#{x}\"; on preceding x"
  (interactive)
  (insert-tranformed-word 
   ruby-word-table 
   (lambda (var) (concat "puts \"" var " = #{" var "}\""))
   "variable name") )

(defun ruby-insert-print-this-inspected ()
  "Do puts \"x=#{x}\"; on preceding x"
  (interactive)
  (insert-tranformed-word 
   ruby-word-table 
   (lambda (var) (concat "puts \"" var " = #{" var ".inspect}\""))
   "variable name") )

(defun hyphen-to-camel-case ()
  (interactive)
  (if (looking-at "-")
      (progn
	(delete-char 1)
	(capitalize-region (point) (+ (point) 1)) ) )
  (search-forward "-")
  (backward-char 1))

(defun ruby-search-for-identifier-at-point ()
  (interactive)
  (let ( (word (word-at word-alpha-table (point))) )
    (if word
	(ruby-search-for-identifier word)
      (call-interactively 'ruby-search-for-identifier) ) ) )

(defun ruby-search-for-identifier (identifier)
  (interactive "sSearch for: ")
  (show-search-buffer (list default-directory) '(".yaml" ".rb" ".haml") identifier) )

(defvar *rails-process* nil "Development Ruby on Rails process")

(defun run-rails-server (run-server-script base-dir)
  (interactive)
  (let ( (ruby-executable (project-value :ruby-executable "ruby")) )
    (switch-to-buffer-other-window "*rails*")
    (clear-buffer)
    (stop-then-start-process "rails" '*rails-process* "*rails*" 
			      run-server-script (list ruby-executable base-dir) ) ) )

(defun ruby-mode-hook-function ()
  (setq expansion-key 'ruby-expansion-key)
  (local-set-key [?\C-t] 'ruby-insert-member-equals)
  (local-set-key [?\C-p] 'ruby-insert-print-this)
  (local-set-key [?\C-\S-p] 'ruby-insert-print-this-inspected)
  (local-set-key [?\C-m] 'return-and-indent)
  (local-set-key [?\C-w] 'ruby-search-for-identifier-at-point)
  (setq run-file-function #'ruby-run-file)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [f2] 'my-expand-abbrev)
  (local-set-key "=" 'insert-spaced-equals)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key [?\C->] 'insert-hash-key)
  (local-set-key [?\C-\M-C] 'hyphen-to-camel-case)
  (font-lock-mode 1)
)

(defun def-ruby-abbrev (abbrev expansion)
  "Define ABBREV to have EXPANSION in ruby mode"
  (set-abbrev abbrev expansion 'ruby-expansion-key) )

(def-ruby-abbrev "cl" '("class " indent mark return indent return "end" indent goto-mark))
(def-ruby-abbrev "reqt" '("require 'test/unit'"))
(def-ruby-abbrev "clt" '("class " indent mark "TestCase < Test::Unit::TestCase" return indent return "end" indent goto-mark))
(def-ruby-abbrev "di" '("def initialize" indent return indent mark return "end" indent goto-mark))
(def-ruby-abbrev "d" '("def " indent mark return "end" indent goto-mark))
(def-ruby-abbrev "if" '("if " indent mark return "end" indent goto-mark))
(def-ruby-abbrev "dt" '("def test" indent mark return "end" indent goto-mark))
(def-ruby-abbrev "do" '("do |" indent mark "|" return "end" indent goto-mark))
(def-ruby-abbrev "e" '("end" indent))
(def-ruby-abbrev "r" '("return "))

(def-ruby-abbrev "for" '("for " indent mark " in  do" return indent return "end" indent goto-mark))

(def-ruby-abbrev "ar" '("attr_reader :" indent))
(def-ruby-abbrev "aa" '("attr_accessor :" indent))

(def-ruby-abbrev "ae" '("assert_equal "))
(def-ruby-abbrev "asr" '("assert_raise (" mark ") {}" goto-mark))

(def-ruby-abbrev "kkk" '(indent "#==============================================================================="))

(def-ruby-abbrev "mod" '("module " indent mark return "end" indent goto-mark))

(def-ruby-abbrev "p" '("puts " indent "\"" mark "\"" goto-mark))

(add-hook 'ruby-mode-hook 'ruby-mode-hook-function)

(global-set-key [?\M-r] 'ruby-visit-output-buffer)
