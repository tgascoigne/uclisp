(defmacro test-expect (expected expr)
  `(progn
     (message "Testing %v expecting %v" ',expr ',expected)
     (let ((result ,expr))
       (if (eq result ,expected)
           (message "Ok")
         (message "Value incorrect: got %v expected %v" result ,expected)
         (set '*test-success* nil)))))

(defun testingp ()
  (defined *testing*))

(defmacro deftests (&rest body)
  `(if (testingp)
       (progn
         (message "Running tests for %v" *load-file-path*)
         ,@body)))

(defmacro run-tests (&rest body)
  `(progn
     (message "Running tests...")
     (define *testing* t)
     (define *test-success* t)
     ,@body
     *test-success*))
