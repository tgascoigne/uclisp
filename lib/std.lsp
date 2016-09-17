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
;; List manipulation
;;

(define cons
  (lambda (car cdr)
    (bytecode (LOAD cdr LOOKUP LOAD car LOOKUP CONS))))

(define car
  (lambda (cons)
    (bytecode (LOAD cons LOOKUP CAR))))

(define cdr
  (lambda (cons)
    (bytecode (LOAD cons LOOKUP CDR))))

(define pairlis
  (lambda (keys values)
    (if (nilp keys)
        ()
      (cons (cons (car keys) (car values))
            (pairlis (cdr keys) (cdr values))))))

(define map
  (lambda (fn sequence)
    (if (nilp sequence)
        '()
      (cons (fn (car sequence)) (map fn (cdr sequence))))))

(define append
  (lambda (list el)
    (if (nilp list)
        (cons el '())
      (cons (car list) (append (cdr list) el)))))

(define prepend
  (lambda (el list)
    (cons el list)))

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

(define add-to-alist
  (lambda (alist key value)
    (set alist (cons (cons key value) (eval alist)))))

;;
;; Compiler (!!)
;;

(define eval
  (lambda (expr)
    (eval-bytecode (compile expr))))

(define eval-bytecode
  (lambda (code)
    (bytecode (LOAD code LOOKUP EVAL))))

(define compile
  (lambda (expr)
    (compile-internal expr ())))

(define compile-internal
  (lambda (expr tail)
    (if (consp expr)
        (compile-sexpr expr tail)
      (compile-const expr tail))))

(define compile-const
  (lambda (expr tail)
    (if (symbolp expr)
        (cons $LOAD (cons expr (cons $LOOKUP tail)))
      (cons $LOAD (cons expr tail)))))

(define compile-sexpr
  (lambda (expr tail)
    (if (eq (car expr) 'quote)
        ;; quote
        (compile-quoted-list (car (cdr expr)) tail)
      (if (eq (car expr) 'backquote)
          ;; backquote
          (compile-bq (car (cdr expr)) tail)
        (if (eq (car expr) 'lambda)
            ;; lambda
            (compile-bq
             `(lambda ,(car (cdr expr))
                ,(compile (car (cdr (cdr expr)))))
             tail)
          (if (assoc (car expr) *macros*)
              (compile-internal (macroexpand expr) tail)
            ;; apply
            (compile-list (cdr expr)
                          (compile-internal (car expr)
                                            (compile-instr $APPLY tail)))))))))

(define compile-instr
  (lambda (instr tail)
    (cons instr tail)))

(define compile-list
  (lambda (list tail)
    (if (consp list)
        (compile-list (cdr list)
                      (compile-internal (car list)
                                        (compile-instr $CONS tail)))
      (compile-internal list tail))))

(define compile-quoted
  (lambda (expr tail)
    (if (consp list)
        (compile-quoted-list expr tail)
      (compile-quoted-const expr tail))))

(define compile-quoted-list
  (lambda (list tail)
    (if (consp list)
        (if (eq (car list) 'unquote)
            (compile-internal (car (cdr list)) tail)
          (compile-quoted-list (cdr list)
                               (compile-quoted (car list)
                                               (compile-instr $CONS tail))))
      (compile-quoted list tail))))

(define compile-quoted-const
  (lambda (expr tail)
    (cons $LOAD (cons expr tail))))

(define compile-bq
  (lambda (expr tail)
    (if (consp expr)
        (compile-bq-list expr tail)
      (compile-quoted expr tail))))

(define compile-bq-is-splice
  (lambda (list)
    (if (consp list)
        (if (eq (car list) 'splice)
            1
          '())
      '())))

(define compile-bq-list
  (lambda (list tail)
    (compile-quoted-list (compile-bq-list-expand list) tail)))

(define compile-bq-list-expand
  (lambda (list)
    (if (consp list)
        (if (compile-bq-is-splice (car list))
            (concat (eval (car (cdr (car list))))
                    (compile-bq-list-expand (cdr list)))
          (if (eq (car list) 'unquote)
              list
            (cons (compile-bq-list-expand (car list)) (compile-bq-list-expand (cdr list)))))
      list)))

(define concat
  (lambda (a b)
    (if (nilp a)
        b
      (cons (car a) (concat (cdr a) b)))))

(define toggle-trace
  (lambda () (set '*trace* (not *trace*))))

;;
;; Macros
;;

(define compile-lambda
  (lambda (func)
    `(lambda ,(car (cdr func)) ,(append (compile (car (cdr (cdr func)))) $RETURN))))

(define genquote
  (lambda (el)
    (cons 'quote (cons el '()))))

(define macroexpand
  (lambda (expr)
    (let ((macro-name (car expr))
          (macro-args (cdr expr)))
      (let ((macro-decl (cdr (assoc macro-name *macros*))))
        (let ((macro-func (cons 'lambda (cdr macro-decl))))
          (eval (prepend macro-func (map genquote macro-args))))))))

(define *macros* '())

(add-to-alist '*macros* 'cadr
              '(macro (el)
                      `(car (cdr ,el))))
