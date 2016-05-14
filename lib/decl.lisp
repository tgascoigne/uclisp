(progn

  (define defmacro
    (macro (symbol args &rest body)
           `(define ,symbol (macro ,args ,@body))))

  (defmacro defun (symbol args &rest body)
    `(define ,symbol (lambda ,args ,@body)))

  (defmacro setq (symbol value)
    `(set ',symbol ,value))

  (cond ((defined *testing*)
         ;; there's an awkward circular dependency between decl.lisp and test/framework.lisp
         ;; do an explicit test for *testing* rather than expecting deftests to be there
         (deftests

           (test-expect 16
                        (progn
                          (defun square (x) (* x x))
                          (square 4)))

           (test-expect 1
                        (let ((x 2))
                          (setq x 1)
                          x))

           )
         ))
  )
