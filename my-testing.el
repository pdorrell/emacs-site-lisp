(defvar *run-tests* t "When t, run tests")

(defvar *running-multiple-tests* nil "When t, running multiple tests")
(defvar *test-results* nil "Passes & fails of tests")

(defmacro run-test (expression expected-value-expression)
  (declare (indent defun))
  `(if *run-tests*
       (let* ( (calculated-value ,expression)
               (expected-value ,expected-value-expression)
               (passed (equal calculated-value expected-value)) )
         (message "*running-multiple-tests* = %S" *running-multiple-tests*)
         (if *running-multiple-tests*
             (let ( (test-report 
                     (if passed
                         (list 'pass ',expression calculated-value)
                       (list 'fail ',expression calculated-value expected-value) ) ) )
               (setq *test-results* (cons test-report *test-results*)) )
           (if passed
             (message "Test passed %S = %S" ',expression expected-value)
             (message "Test failure %S != expected %S" calculated-value expected-value) ) ) ) ) )

(defun show-test-results (label test-results)
  (let ( (pass-count 0)
         (fail-count 0)
         (total-count (length test-results))
         (test-result-buffer (get-buffer-create "*test-results*")) )
    (if (equal total-count 0)
        (message label)
      (progn
        (dolist (test-result test-results)
          (let ( (result-type (first test-result)) )
            (cond
             ((eq result-type 'pass)
              (setq pass-count (+ pass-count 1)))
             ((eq result-type 'fail)
              (setq fail-count (+ fail-count 1)))) ) )
        (if (equal fail-count 0)
            (message "%s, %S tests passed" label pass-count)
          (let ( (test-result-buffer (get-buffer-create "*test-results*")) )
            (switch-to-buffer-other-window test-result-buffer)
            (delete-region (point-min) (point-max))
            (dolist (test-result (reverse test-results))
              (if (eq (first test-result) 'fail)
                  (cl-destructuring-bind (expression calculated-result expected-result) (cdr test-result)
                    (insert (format "FAIL %S = %S, not expected %S\n\n"
                                    expression calculated-result expected-result)) ) ) )
            (beginning-of-buffer)
            (message "%s, tests: %S passed, %S failed" label pass-count fail-count) ) ) ) ) ) )

(defmacro running-tests (label &rest body)
  `(let ( (*running-multiple-tests* t) 
          (*test-results* nil) )
     ,@body
    (if *run-tests*
        (show-test-results ,label *test-results*) ) ) )

