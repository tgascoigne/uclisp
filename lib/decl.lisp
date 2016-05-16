(define defmacro
  (macro (symbol args &rest body)
         `(define ,symbol (macro ,args ,@body))))

(defmacro defun (symbol args &rest body)
  `(define ,symbol (lambda ,args ,@body)))

(defmacro setq (symbol value)
  `(set ',symbol ,value))
