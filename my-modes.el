(defun update-auto-mode-alist (regex-pattern mode)
  "Add REGEX-PATTERN & MODE to auto-mode-alist"
  (push (cons regex-pattern mode) auto-mode-alist) )

(defun set-extension-mode (extension mode)
  "Set emacs MODE for file EXTENSION (eg '.py')"
  (update-auto-mode-alist (concat "\\" extension "\\'") mode) )

(defun set-file-name-mode (file-name mode)
  "Set emacs MODE for FILE-NAME"
  (update-auto-mode-alist (concat "[/]" (regexp-quote file-name) "\\'") mode) )

(apply-to-list-of-arg-lists 'set-file-name-mode
  '( ("_"        file-menu-mode)
     ) )

(apply-to-list-of-arg-lists 'set-extension-mode
  '( (".song" melody-scripter-mode)
     (".file-menu" file-menu-mode)
     
     (".sh" bash-ts-mode)
     (".js" js-ts-mode)
     (".json" json-ts-mode)
     (".jsx" tsx-ts-mode)
     (".css" css-ts-mode)
     (".html" mhtml-mode)
     (".ruby" ruby-ts-mode)
     (".rust" rust-ts-mode)
     (".scss" scss-mode)
     (".toml" toml-ts-mode)
     (".ts" typescript-ts-mode)
     (".tsx" tsx-ts-mode)
     (".yaml" yaml-ts-mode)
     ) )
