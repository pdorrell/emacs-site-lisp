;; Add TypeScript-specific sources to the default list

(require 'treesit)

(defvar *treesitter-sources* 
  '(
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")
  ) )

(dolist (treesitter-source *treesitter-sources*)
  (add-to-list 'treesit-language-source-alist treesitter-source) )

(defvar *treesitter-langs* '(bash typescript tsx python javascript css html json toml yaml))

(dolist (lang *treesitter-langs*)
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang) ) )
