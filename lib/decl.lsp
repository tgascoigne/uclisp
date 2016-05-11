(progn

  (define defmacro
    (macro (symbol args body)
           `(define ,symbol (macro ,args (,@body)))))

  (defmacro defun (symbol args body)
    `(define ,symbol (lambda ,args (,@body))))

  )
