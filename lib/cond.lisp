(progn

  (defmacro if (condition then &rest else)
    `(cond
      (,condition ,then)
      (t ,@else)))

  (defmacro unless (condition &rest body)
    `(if ,condition
         ()
       ,@body))


  (load-file "lib/test/framework.lisp")
  (deftests

    (test-expect 4
                 (if t
                     (* 2 2)
                   (* 4 4))
                 )

    (test-expect 16
                 (if nil
                     (* 2 2)
                   (* 4 4))
                 )

    (test-expect 1
                 (unless (= 1 2) 1))

    (test-expect nil
                 (unless (= 1 1) 1))

    )

  )
