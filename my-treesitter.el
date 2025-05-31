;; Add TypeScript-specific sources to the default list
(add-to-list 'treesit-language-source-alist 
             '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
(add-to-list 'treesit-language-source-alist 
             '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))

;; Install if not available
(unless (treesit-language-available-p 'typescript)
  (treesit-install-language-grammar 'typescript))
(unless (treesit-language-available-p 'tsx)
  (treesit-install-language-grammar 'tsx))


(add-to-list 'treesit-language-source-alist 
             '(scss "https://github.com/serenadeai/tree-sitter-scss"))

;; Install if not available
(unless (treesit-language-available-p 'scss)
  (treesit-install-language-grammar 'scss))

(treesit-install-language-grammar 'scss)

(treesit-language-available-p 'scss)

