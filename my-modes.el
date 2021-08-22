(defun set-extension-mode (extension mode)
  (setq auto-mode-alist
        (cons (cons (concat "\\" extension "\\'") mode)
              auto-mode-alist) ) )

(defun set-file-name-mode (file-name mode)
  (setq auto-mode-alist
        (cons (cons (concat "[/]" (regexp-quote file-name) "\\'") mode)
              auto-mode-alist) ) )

(apply-to-list-of-arg-lists
 'set-file-name-mode
 '(
   ("_"        file-menu-mode)
   ("rakefile" ruby-mode)
   ("Rakefile" ruby-mode)
   ) )

(apply-to-list-of-arg-lists
 'set-extension-mode
 '(
   (".egg"  archive-mode)
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
   ) )
