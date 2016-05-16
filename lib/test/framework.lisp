(define test-set '())

(defmacro test-expect (expected expr)
  `(progn
     (message "Testing %v expecting %v" ',expr ',expected)
     (let ((result ,expr))
       (if (eq result ,expected)
           (message "Ok")
         (message "Value incorrect: got %v expected %v" result ,expected)
         (set '*test-success* nil)))))

(defmacro deftests (&rest body)
  `(setq test-set (append test-set `((,*load-file-path* ,body)))))

(defun run-tests ()
  (progn
     (define *testing* t)
     (define *test-success* t)
     (dolist (tests test-set *test-success*)
       (let ((lisp-file (nth 0 tests))
             (test-funcs (nth 1 tests)))
         (message "Testing %v..." lisp-file)
         (dolist (test test-funcs *test-success*)
           (eval test))))))
