(load "compiler-sbcl.lisp")

(loop for expr = (read *standard-input* nil :eof)
      until (eq expr :eof)
      do (format t "~a~%" (fasl-print (ucl-compile expr))))
