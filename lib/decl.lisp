(define docstring-alist '())

(define decl--parse-docstring
  (lambda (symbol body)
         (cond
          ((stringp (car body))
           (set 'docstring-alist (append `((,symbol ,(car body))) docstring-alist))
           (set 'body (cdr body))))
         body))

(define defmacro
  (macro (symbol args &rest body)
         (set 'body (decl--parse-docstring symbol body))
         `(define ,symbol (macro ,args ,@body))))

(defmacro defun (symbol args &rest body)
  "Define a new function with the name `symbol', which takes `args' as an argument list and executes `body'.

The argument specification `args' supports the following CL style &optional and &rest argument lists."
  (set 'body (decl--parse-docstring symbol body))
  `(define ,symbol (lambda ,args ,@body)))

(defmacro setq (symbol value)
  "Set `symbol' to `value'. `symbol' is taken as a literal symbol, and so it does not need to be quoted."
  `(set ',symbol ,value))
