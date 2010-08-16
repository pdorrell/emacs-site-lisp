(autoload 'haxe-mode "haxe-mode")

(set-extension-mode ".hx" 'haxe-mode)

(setq haxe-word-table
      (make-alpha-table letters-digits-string "_") )

(defun make-haxe-for-loop ()
  "Make for loop using preceding variable name"
  (interactive)
  (let ( (var (word-before haxe-word-table (point)))
	 saved-point)
    (if var
	(progn
	  (delete-backward-char (length var))
	  (insert "for (int " var "=0; " var "<")
	  (setq saved-point (point))
	  (insert "; " var "++) {\n")
	  (indent-for-tab-command)
	  (insert "\n}") (indent-for-tab-command)
	  (goto-char saved-point) )
      (message "No variable name given for for loop") ) ) )

(add-hook 'haxe-mode-hook 'my-haxe-hook)

(defun my-haxe-hook ()
  (setq expansion-key 'haxe-expansion-key)
  (local-set-key [?\C-m] 'java-return)
  (setq c-basic-offset 2)
  (local-set-key [?\C-f] 'make-for-loop)
  (local-set-key [?\C-t] 'insert-this-equals)
  (local-set-key "=" 'insert-spaced-equals)
  (local-set-key [?\C-=] 'insert-equals)
  (local-set-key "," 'insert-spaced-comma)
  (local-set-key [f2] 'my-expand-abbrev)
  (local-set-key [f10] 'shift-initial-case)
  (setq word-alpha-table haxe-word-table)
  (setq indent-tabs-mode nil)
  (font-lock-mode 1)
  (setq comment-start "/*")
  (setq comment-end "*/") )

(defun def-haxe-abbrev (abbrev expansion)
  "Define ABBREV to have EXPANSION in haxe mode"
  (set-abbrev abbrev expansion 'haxe-expansion-key) )

;;-----------------------------------------------------------------
(def-haxe-abbrev "p" '("public "))
(def-haxe-abbrev "pf" '("public function "))

(def-haxe-abbrev "pfn" '("public function new(" mark ")" goto-mark))

(def-haxe-abbrev "i" '("Int"))
(def-haxe-abbrev "b" '("Bool"))
(def-haxe-abbrev "v" '("var "))
(def-haxe-abbrev "vd" '("Void"))
(def-haxe-abbrev "s" '("String"))
(def-haxe-abbrev "st" '("static "))
(def-haxe-abbrev "sv" '("static var "))

(def-haxe-abbrev "psf" '("public static function "))
(def-haxe-abbrev "pv" '("public var "))
(def-haxe-abbrev "d" '("Dynamic"))

(def-haxe-abbrev "al" '("js.Lib.alert(\"" mark "\");" goto-mark))

(def-haxe-abbrev "r" "return ")
(def-haxe-abbrev "ims" "implements ")
(def-haxe-abbrev "int" "interface ")
(def-haxe-abbrev "tr" '("trace(\"" mark "\");" goto-mark) )
