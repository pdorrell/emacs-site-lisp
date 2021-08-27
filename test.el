
(cl-destructuring-bind (a b c &optional d) '(1 2 3)
  (message "%S" d))
