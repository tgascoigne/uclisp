(defmacro dolist (spec &rest body)
  "Iterate through a list.

`spec' is of the form (symbol list &optional result). Loop through `list', assign the current value to `symbol', and execute `body'. Loop until the end of `list'

Optionally evaluate `result' as the result"
  (let ((var (nth 0 spec))
        (result (nth 2 spec)))
    `(let ((inlist ,(nth 1 spec)))
       (while (car inlist)
         (let ((,var (car inlist)))
           ,@body
           (setq inlist (cdr inlist))))
       ,result)))

(defmacro docount (spec &rest body)
  "Iterate through a sequence of integers.

`spec' is of the form (symbol count &optional result). Loop through the sequence 0..`count', assign the current value to `symbol', and execute `body'. Loop until the end of the sequence.

Optionally evaluate `result' as the result"
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
                 (defun sumn (x)
                   (let ((sum 0))
                     (docount (i x sum)
                              (inc sum i))))
                 (sumn 4))) ; 0+1+2+3 = 6

  )
