(progn
  (load-file "lib/test/framework.lisp")
  (run-tests
   ;; decl is a special case, because there's a circular dependency between it and framework.lisp
;   (load-file "lib/decl_test.lisp")
   (load-file "lib/std.lisp")
   )
  )
