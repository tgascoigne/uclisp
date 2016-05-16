;; test/framework.lisp depends on decl.lisp, so i've split the tests out into a separate
;; file, to be loaded after both decl and framework.lisp
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
