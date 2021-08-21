
(defvar *sass-watch-process* nil "SASS watch process")

(defun sass-watch(src-output-argument)
  (interactive)
  (let ( (ruby-executable (project-value :ruby-executable "ruby")) )
    (switch-to-buffer-other-window "*sass-watch*")
    (clear-buffer)
    (stop-then-start-process "sass-watch" '*sass-watch-process* "*sass-watch*"
			      ruby-executable (list "-S" "sass" "--unix-newlines" "--watch" src-output-argument) ) ) )

(defun run-sass-watch-command()
  (interactive)
  (let ( (src-output-argument (project-required-value :sass-watch-src-output-argument)) )
    (sass-watch src-output-argument) ) )
			     
(set-extension-mode ".scss" 'css-mode)
