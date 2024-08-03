(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "<mouse-3>") nil)
  (define-key lsp-mode-map (kbd "<S-down-mouse-1>") 'lsp-mouse-click) )
