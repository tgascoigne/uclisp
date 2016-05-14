(progn
  (load-file "lib/test/framework.lisp")
  (deftests

    (test-expect 1
                 (let ((x nil))
                   (setq x 1)
                   x))

    )
  )
