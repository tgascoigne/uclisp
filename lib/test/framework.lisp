(defmacro test-expect (expected expr)
  `(progn
     (message "Testing %v expecting %v" ',expr ',expected)
     (let ((result ,expr))
       (if (eq result ,expected)
           (message "Ok")
         (message "Value incorrect: got %v expected %v" result ,expected)
         (set '*test-success* nil))
       )))

(defmacro deftests (&rest body)
  `(if (defined *testing*)
       (progn ,@body)))

(defmacro run-tests (&rest body)
  `(progn
     (message "Running tests...")
     (define *testing* t)
     (define *test-success* t)
     ,@body
     *test-success*))
