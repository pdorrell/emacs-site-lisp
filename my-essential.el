;; Copyright (C) 2000,2001 Philip Dorrell

(require 'debug)
(require 'cl-print)

;; Code deemed more 'essential', to be loaded early on in case other code has an error loading.

;;========================================================================
(defun toggle-debug ()
  "Toggle debug-on-error"
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "Set debug %s" (if debug-on-error "on" "off")) )

(defun save-some-buffers-and-eval-defun()
  "For some reason this doesn't work, due to non-standard arguments to eval-defun"
  (interactive)
  (if (buffer-file-name) (save-buffer))
  (save-some-buffers)
  (eval-defun) )

;;========================================================================
(global-set-key [f9] 'eval-defun)
(global-set-key [S-f9] 'eval-region)

(global-set-key [S-f12] 'toggle-debug-on-error)
