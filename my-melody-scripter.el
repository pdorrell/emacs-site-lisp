
(autoload 'melody-scripter-mode "melody-scripter-mode")

(set-extension-mode ".song" 'melody-scripter-mode)

(add-hook 'melody-scripter-mode-hook 'my-melody-scripter-hook)

(defun play-this-song-file(song_file)
  (let ( (filename (windowize-filename song_file)) )
    (condition-case nil (kill-buffer "*play-song*") (error nil))
    (setq *related-output-process*
          (start-process "play-song" "*play-song*" "~/bin/play-song" filename) )
    (display-buffer "*play-song*") ) )

(defun my-melody-scripter-hook ()
  (setq run-file-function 'play-this-song-file)
  )
