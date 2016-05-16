(progn

  (defmacro dolist (spec &rest body)
    (let ((var (nth 0 spec))
          (result (nth 2 spec)))
      `(let ((inlist ,(nth 1 spec)))
         (while (car inlist)
           (let ((,var (car inlist)))
             ,@body
             (setq inlist (cdr inlist))))
         ,result)))

  (defmacro docount (spec &rest body)
    (let ((var (nth 0 spec))
          (count (nth 1 spec))
          (result (nth 2 spec)))
      `(let ((,var 0))
         (while (< ,var ,count)
           ,@body
           (inc ,var))
         ,result)))

  (deftests

    (test-expect 6
                 (progn
                   (defun sum (x)
                     (let ((sum 0))
                       (dolist (i x sum)
                         (inc sum i))))
                   (sum '(1 2 3))))

    (test-expect 6
                 (progn
;                   (define *macro-debug* t)
                   (defun sumn (x)
                     (let ((sum 0))
                       (docount (i x sum)
                                (inc sum i))))
                   (sumn 4))) ; 0+1+2+3 = 6

    )

  )
