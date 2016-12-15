;;
;; Arithmetic
;;
(define +
  (lambda (a b)
    (bytecode ($LOAD a $LOOKUP $LOAD b $LOOKUP $ADD))))

(define -
  (lambda (a b)
    (bytecode ($LOAD a $LOOKUP $LOAD b $LOOKUP $SUB))))

(define *
  (lambda (a b)
    (bytecode ($LOAD a $LOOKUP $LOAD b $LOOKUP $MUL))))

(define /
  (lambda (a b)
    (bytecode ($LOAD a $LOOKUP $LOAD b $LOOKUP $DIV))))

(define %
  (lambda (a b)
    (bytecode ($LOAD a $LOOKUP $LOAD b $LOOKUP $MOD))))

;;
;; Predicates
;;

(define t 1)

(define and
  (lambda (c1 c2)
    (if c1
        (if c2
            t
          nil)
      nil)))

(define typeof
  (lambda (obj)
    (bytecode ($LOAD obj $LOOKUP $TYPE))))

(define eq
  (lambda (a b)
    (bytecode ($LOAD a $LOOKUP $LOAD b $LOOKUP $EQUAL))))

(define consp
  (lambda (obj)
    (eq (typeof obj) 'cons)))

(define nilp
  (lambda (obj)
    (eq obj '())))

(define symbolp
  (lambda (obj)
    (eq (typeof obj) 'symbol)))

(define not
  (lambda (x)
    (if x
        '()
      1)))

;;
;; I/O
;;

(define read
  (lambda ()
    (bytecode ($READ))))

(define print
  (lambda (el)
    (bytecode ($LOAD el $LOOKUP $PRINT $LOAD el $LOOKUP))))

;;
;; List manipulation
;;

(define cons
  (lambda (car cdr)
    (bytecode ($LOAD cdr $LOOKUP $LOAD car $LOOKUP $CONS))))

(define car
  (lambda (cons)
    (bytecode ($LOAD cons $LOOKUP $CAR))))

(define cdr
  (lambda (cons)
    (bytecode ($LOAD cons $LOOKUP $CDR))))

(define setcar
  (lambda (cell value)
    (bytecode ($LOAD value $LOOKUP $LOAD cell $LOOKUP $SETCAR))))

(define setcdr
  (lambda (cell value)
    (bytecode ($LOAD value $LOOKUP $LOAD cell $LOOKUP $SETCDR))))

(define list
  (lambda (&rest list)
    list))

(define pairlis
  (lambda (keys values)
    (if (nilp keys)
        ()
      (cons (cons (car keys) (car values))
            (pairlis (cdr keys) (cdr values))))))

(define pairargs
  (lambda (keys values)
    (if (nilp keys)
        ()
      (if (eq (car keys) '&rest)
          (cons (cons (car (cdr keys)) values)
                ())
        (cons (cons (car keys) (car values))
              (pairargs (cdr keys) (cdr values)))))))

(define ucl-map
  (lambda (fn sequence)
    (if (nilp sequence)
        '()
      (cons (fn (car sequence)) (ucl-map fn (cdr sequence))))))

(define append
  (lambda (list el)
    (if (nilp list)
        (cons el '())
      (cons (car list) (append (cdr list) el)))))

(define prepend
  (lambda (el list)
    (cons el list)))

(define concat
  (lambda (a b)
    (if (nilp a)
        b
      (cons (car a) (concat (cdr a) b)))))

;;
;; Association lists
;;

(define assoc
  (lambda (key list)
    (if (nilp list)
        ()
      (if (eq (car (car list)) key)
          (car list)
        (assoc key (cdr list))))))

(define assoc-default
  (lambda (key list)
    (cdr (assoc key list))))

(define add-to-alist
  (lambda (alist key value)
    (set alist (cons (cons key value) (eval alist)))))

;;
;; REPL
;;

(define toggle-trace
  (lambda () (set '*trace* (not *trace*))))

(define *ltrace* ())
(define toggle-ltrace
  (lambda () (set '*ltrace* (not *ltrace*))))

(define repl
  (lambda ()
    (progn
      (print (eval (read)))
      (repl))))

;(repl)

(define ucl-apply
    (lambda (fn &rest args)
      (progn
        (let ((bindings (pairargs (car (cdr fn)) args)))
          (dapply fn bindings)))))

(define dapply
    (lambda (fn bindings)
      (bytecode ($LOAD bindings $LOOKUP
                       $LOAD fn $LOOKUP
                       $DAPPLY))))
