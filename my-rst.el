(add-hook 'rst-mode-hook 'rst-mode-hook-function)

(defun rst-mode-hook-function ()
  (setq run-file-function #'rst-compile-to-html5)
  (font-lock-mode 1) )
  
(defun rst-compile-to-html5(rst_file)
  (interactive)
  (let* ( (filename (windowize-filename rst_file))
          (html-file-name (concat filename ".html")) )
    (condition-case nil (kill-buffer "*rst-compile*") (error nil))
    (setq *related-output-process*
          (call-process "rst2html5" nil "*rst-compile*" nil filename html-file-name) )
    (display-buffer "*rst-compile*") ) )
