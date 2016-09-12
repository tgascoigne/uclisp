;;
;; Arithmetic
;;
(define +
  (lambda (a b)
    (bytecode (LOAD a LOOKUP LOAD b LOOKUP ADD))))

(define -
  (lambda (a b)
    (bytecode (LOAD a LOOKUP LOAD b LOOKUP SUB))))

(define *
  (lambda (a b)
    (bytecode (LOAD a LOOKUP LOAD b LOOKUP MUL))))

(define /
  (lambda (a b)
    (bytecode (LOAD a LOOKUP LOAD b LOOKUP DIV))))

(define %
  (lambda (a b)
    (bytecode (LOAD a LOOKUP LOAD b LOOKUP MOD))))

;;
;; Predicates
;;

(define typeof
  (lambda (obj)
    (bytecode (LOAD obj LOOKUP TYPE))))

(define eq
  (lambda (a b)
    (bytecode (LOAD a LOOKUP LOAD b LOOKUP EQUAL))))

(define consp
  (lambda (obj)
    (eq (typeof obj) (quote cons))))

(define nilp
  (lambda (obj)
    (eq obj (quote ()))))

(define cons
  (lambda (car cdr)
    (bytecode (LOAD cdr LOOKUP LOAD car LOOKUP CONS))))

(define car
  (lambda (cons)
    (bytecode (LOAD cons LOOKUP CAR))))

(define cdr
  (lambda (cons)
    (bytecode (LOAD cons LOOKUP CDR))))

(define compile
  (lambda (expr)
    (compile-internal expr ())))

(define compile-internal
  (lambda (expr tail)
    (if (consp expr)
        (compile-sexpr expr tail)
      (cons $LOAD (cons expr tail)))))

(define compile-sexpr
  (lambda (expr tail)
    (compile-list (cdr expr)
                  (compile-internal (car expr)
                           (compile-instr $APPLY tail)))))

(define compile-instr
  (lambda (instr tail)
    (cons instr tail)))

(define compile-list
  (lambda (list tail)
    (if (consp list)
        (compile-list (cdr list)
                      (compile-list (car list)
                                    (compile-instr $CONS tail)))
      (compile-internal list tail))))

;(cons OpLOAD (cons () (cons OpLOAD (cons 1 (cons OpLOAD (cons 1 (cons OpLOAD (cons +))))))))

;((((OpLOAD ()) OpLOAD 1) OpLOAD 1) OpLOAD +)
