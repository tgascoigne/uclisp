;; helper function to access deeply nested structures
;; example:
;;   (-> '(foo bar X))
;; is equivalent to the Go expression
;;   foo.bar.X
;;

(define -> (lambda (path)
             (let ((thistok (car path)))
               (if (= nil (cdr path))
                   thistok
                   (with thistok
                         (-> (cdr path)))))))

;; helper function to set values in deeply nested structures
;; example:
;;  (set-> '(foo bar X) 20)
;; is equivalent to the Go statement
;;  foo.bar.X = 20
;;
;; fixme

(define set-> (lambda (path val)
             (let ((thistok (car path)))
               (if (= nil (cdr path))
                   (set thistok val)
                   (with thistok
                         (set-> (cdr path) val))))))
