;; as suggested by BingChat

(use-package lsp-mode
  :ensure t
  :hook ((typescript-mode . lsp)
         (tsx-mode . lsp)
         (js-mode . lsp)
         (jsx-mode . lsp)
         (css-mode . lsp)
         (html-mode . lsp))
  :commands lsp)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp))

(use-package web-mode
  :ensure t
  :mode ("\\.tsx\\'" . web-mode)
        ("\\.jsx\\'" . web-mode)
        ("\\.html\\'" . web-mode)
        ("\\.css\\'" . web-mode)
  :hook ((web-mode . lsp)
         (web-mode . (lambda ()
                       (when (string-equal "tsx" (file-name-extension buffer-file-name))
                         (setup-tide-mode))))))
