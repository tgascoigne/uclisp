(defmacro if (condition then &rest else)
  "If `condition' is non-nil, `then' is evaluated. Otherwise, `else' is evaluated when present."
  (let ((result (list 'cond (list condition then))))
    (cond (else (setq result (append result (list `(t (progn ,@else)))))))
    result))

(defmacro unless (condition &rest body)
  "Evaluates `body' unless `condition' is non-nil"
  `(if ,condition
       ()
     ,@body))

(defmacro when (condition &rest body)
  "Evaluates `body' when `condition' is non-nil"
  `(if ,condition
       (progn ,@body)))

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

  (test-expect 3
               (unless (= 1 2) 1 2 3))

  (test-expect nil
               (unless (= 1 1) 1))

  (test-expect 1
               (when t 1))

  (test-expect 3
               (when t 1 2 3))

  (test-expect nil
               (when nil 1 2 3))

  )
