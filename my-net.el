

(defun plink (userAtHost)
  "Login in to user@host using plink (assuming keys already loaded into Pageant)"
  (interactive)
  (let ( (buffer-name (format "login-%s" userAtHost)) )
    (make-comint buffer-name "plink.exe" nil
		 userAtHost)
    (switch-to-buffer (concat "*" buffer-name "*")) ) )

