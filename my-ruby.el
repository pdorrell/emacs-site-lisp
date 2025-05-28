(setq ruby-word-table
      (make-alpha-table letters-digits-string "_") )

(defun insert-hash-key ()
  (interactive)
  (insert " => ") )

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

(defun ruby-mode-hook-function ()
  (setq programming-language 'ruby)
  (local-set-key [?\C-t] 'ruby-insert-member-equals)
  (local-set-key [?\C-p] 'ruby-insert-print-this)
  (local-set-key [?\C-\S-p] 'ruby-insert-print-this-inspected)
  (local-set-key [?\C-m] 'return-and-indent)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [f2] 'my-expand-abbrev)
  (local-set-key "=" 'insert-spaced-equals)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key [?\C->] 'insert-hash-key)
  (local-set-key [?\C-\M-C] 'hyphen-to-camel-case)
  (font-lock-mode 1)
)

(add-hook 'ruby-mode-hook 'ruby-mode-hook-function)

(set-abbrev-language 'ruby)

(set-abbrevs 
 'ruby
 '(
   ("cl" ("class " indent mark return indent return "end" indent goto-mark))
   ("reqt" ("require 'test/unit'"))
   ("clt" ("class " indent mark "TestCase < Test::Unit::TestCase" return indent return "end" indent goto-mark))
   ("di" ("def initialize" indent return indent mark return "end" indent goto-mark))
   ("d" ("def " indent mark return "end" indent goto-mark))
   ("if" ("if " indent mark return "end" indent goto-mark))
   ("dt" ("def test" indent mark return "end" indent goto-mark))
   ("do" ("do |" indent mark "|" return "end" indent goto-mark))
   ("e" ("end" indent))
   ("r" ("return "))

   ("for" ("for " indent mark " in  do" return indent return "end" indent goto-mark))

   ("ar" ("attr_reader :" indent))
   ("aa" ("attr_accessor :" indent))

   ("ae" ("assert_equal "))
   ("asr" ("assert_raise (" mark ") {}" goto-mark))

   ("kkk" (indent "#==============================================================================="))

   ("mod" ("module " indent mark return "end" indent goto-mark))

   ("p" ("puts " indent "\"" mark "\"" goto-mark))

   ("describe" ("describe " indent "\"" mark "\" do" return indent return "end" indent goto-mark))
   ("it" ("it " indent "\"" mark "\" do" return indent return "end" indent goto-mark))

   ("utf" ("# -*- coding: utf-8 -*-")) ) )
