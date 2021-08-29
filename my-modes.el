(defun update-auto-mode-alist (regex-pattern mode)
  (push (cons regex-pattern mode) auto-mode-alist) )

(defun set-extension-mode (extension mode)
  (update-auto-mode-alist (concat "\\" extension "\\'") mode) )

(defun set-file-name-mode (file-name mode)
  (update-auto-mode-alist (concat "[/]" (regexp-quote file-name) "\\'") mode) )

(apply-to-list-of-arg-lists 'set-file-name-mode
  '( ("_"        file-menu-mode)
     ("rakefile" ruby-mode)
     ("Rakefile" ruby-mode)
     ) )

(apply-to-list-of-arg-lists 'set-extension-mode
  '( (".egg"  archive-mode)
     (".erb"  web-mode)
     (".html" web-mode)
     (".js"   javascript-mode)
     (".json" javascript-mode)
     (".jsx"  rjsx-mode)
     (".rb"   ruby-mode)
     (".scss" css-mode)
     (".song" melody-scripter-mode)
     (".ts"   typescript-mode)
     (".tsx"  rjsx-mode)
     (".yml"  yaml-mode)
     (".file-menu" file-menu-mode)
     ) )
