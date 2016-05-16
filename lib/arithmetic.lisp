(defmacro inc (sym &optional count)
  "increments `sym' by `count'.

`count' is optional, and defaults to 1"
  (unless count (setq count 1))
  `(progn
     (setq ,sym (+ ,sym ,count))
     ,sym))

(defmacro dec (sym &optional count)
  "decrements `sym' by `count'.

`count' is optional, and defaults to 1"
  (unless count (setq count 1))
  `(progn
     (setq ,sym (- ,sym ,count))
     ,sym))

(deftests

  (test-expect 2
               (let ((x 1))
                 (inc x)))

  (test-expect 3
               (let ((x 1))
                 (inc x 2)))

  (test-expect 9
               (let ((x 10))
                 (dec x)))

  (test-expect 8
               (let ((x 10))
                 (dec x 2)))

  )
