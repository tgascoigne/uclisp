(defun list-contains-p (list element)
  "Evaluates to non-nil if `list' contains `element'"
  (cond
   ((eq list nil) nil)
   ((eq (car list) element) t)
   (t (list-contains-p (cdr list) element))))

(defun add-to-list (listsym element)
  "Adds `element' to the list `listsym' only if `listsym' doesn't already contain `element'"
  (unless (list-contains-p (eval listsym) element)
    (set listsym (append (eval listsym) (list element)))))

(defun reverse (list)
  "Reverses `list'"
  (let (value)
    (dolist (elt list value)
      (setq value (append `(,elt) value)))))

(deftests

  (test-expect t
               (list-contains-p '(1 2 3) 2))

  (test-expect t
               (list-contains-p '(1 2 3 4) 3))

  (test-expect t
               (list-contains-p '(1 2 "x") "x"))

  (test-expect t
               (list-contains-p `(1 2 ,(* 2 2)) 4))

  (test-expect '(1 2 3 4)
               (let ((foo '(1 2 3)))
                 (add-to-list 'foo 4)
                 foo))

  (test-expect '(1 2 3 4)
               (let ((foo '(1 2 3)))
                 (add-to-list 'foo 4)
                 (add-to-list 'foo 4)
                 foo))

  (test-expect '(1 2 3)
               (let ((foo '(1 2 3)))
                 (add-to-list 'foo 1)
                 foo))

  (test-expect '(3 2 1)
               (reverse '(1 2 3)))

  )
