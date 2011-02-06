

(defun plink (userAtHost)
  "Login in to user@host using plink (assuming keys already loaded into Pageant)"
  (interactive)
  (let ( (buffer-name (format "plink-%s" userAtHost)) )
    (make-comint buffer-name "plink.exe" nil
		 userAtHost)
    (switch-to-buffer (concat "*" buffer-name "*")) ) )

(defun ssh (userAtHost)
  "Login in to user@host using ssh bash (assuming default key)"
  (interactive)
  (let ( (buffer-name (format "ssh-bash-%s" userAtHost)) )
    (make-comint buffer-name "ssh.exe" nil
		 "-t" userAtHost "bash")
    (switch-to-buffer (concat "*" buffer-name "*")) ) )

