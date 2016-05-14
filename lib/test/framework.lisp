(progn

  (defmacro test-expect (expected expr)
    `(progn
       (printf "Testing %v expecting %v" ',expr ',expected)
       (let ((result ,expr))
         (unless (eq result ,expected)
           (printf "Value incorrect: got %v expected %v" result ,expected)
           (set '*test-success* nil))
         ))
    )

  (defmacro deftests (&rest body)
    `(if (defined *testing*)
         (progn ,@body)))

  (defmacro run-tests (&rest body)
    `(progn
       (printf "Running tests...")
       (define *testing* t)
       (define *test-success* t)
       ,@body
       *test-success*))

  )
