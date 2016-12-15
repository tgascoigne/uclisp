(load "compiler-sbcl.lisp")

(defun slurp-stream (stream)
  (let ((contents (make-string (file-length stream))))
    (read-sequence contents stream)
    contents))

(defun uclisp-eval (expr)
  (let* ((compiled (fasl-print (ucl-compile expr)))
         (process (run-program "/Users/tom/go/bin/uclisp"
                         (list "-eval" compiled "std.lisp")
                         :input nil :output :stream))
         (s (process-output process))
         (code (process-exit-code process))
         (output (read-line s nil)));; (read s nil nil)
    (format t "~a~%~~ ~a~%=> ~a~%~%" expr compiled output)
    (if (equal code 0)
        (read-from-string output)
      'error)))

(defun uclisp-test (expr expected)
  (let ((result (uclisp-eval expr)))
    (unless (equal result expected)
      (format t "Test case failed: ~a -> ~a~%expected ~a~%" expr result expected))))

(uclisp-test '(+ 1 2) 3)
(uclisp-test '(+ 1 (* 2 3)) 7)
(uclisp-test '(quote foo) 'foo)
(uclisp-test ''(foo bar) '(foo bar))
(uclisp-test '(if 1 'foo 'bar) 'foo)
(uclisp-test '(if nil 'foo 'bar) 'bar)

(uclisp-test '(progn
               (defun foo (a b)
                 (+ a b))
               (foo 5 3))
             8)

(uclisp-test '(progn
               (define foo 5)
               foo)
             5)

(uclisp-test '(progn
               (define foo 5)
               (set foo (* foo foo))
               foo)
             25)

(uclisp-test '((lambda (a b) (* a b)) 4 2)
             8)

(uclisp-test '((lambda (a b &rest c) c) 1 2 3 4 5)
             '(3 4 5))

(uclisp-test '(let ((x 3) (y 2)) (* x y))
             6)

(uclisp-test '(let* ((foo 3) (bar (* foo 2))) (* bar 2))
             12)

(uclisp-test '(bytecode ($LOAD 2 $LOAD 4 $MUL))
             8)
