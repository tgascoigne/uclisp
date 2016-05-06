(define with-> (lambda (with-path prog)
                 (let ((thistok (car with-path)))
                   (with thistok
                         (if (= nil (cdr with-path))
                             (funcall prog)
                             (with-> (cdr with-path) prog))))))

;; helper function to access deeply nested structures
;; example:
;;   (-> '(foo bar X))
;; is equivalent to the Go expression
;;   foo.bar.X
;;

(define -> (lambda (path)
             (with-> (butlast path)
               (lambda ()
                 (last path)))))

;; helper function to set values in deeply nested structures
;; example:
;;  (set-> '(foo bar X) 20)
;; is equivalent to the Go statement
;;  foo.bar.X = 20
;;

(define set-> (lambda (path val)
             (with-> (butlast path)
               (lambda ()
                 (message "last path is %v" (last path))
                 (set (symbol (last path)) val)))))

(message "Z is %v" (-> '(foo Bar Z)))
(set-> '(foo Bar Z) 20)
(message "Z is %v" (-> '(foo Bar Z)))
