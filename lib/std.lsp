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

(define t 1)

(define nil '())

(define and
  (lambda (c1 c2)
    (if c1
        (if c2
            t
          nil)
      nil)))

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
;; I/O
;;

(define read
  (lambda ()
    (bytecode (READ))))

(define print
  (lambda (el)
    (bytecode (LOAD el LOOKUP PRINT LOAD el LOOKUP))))

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

(define setcar
  (lambda (cell value)
    (bytecode (LOAD value LOOKUP LOAD cell LOOKUP SETCAR))))

(define setcdr
  (lambda (cell value)
    (bytecode (LOAD value LOOKUP LOAD cell LOOKUP SETCDR))))

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
;; Compiler (!!)
;;

(define apply
  (lambda (fn args)
    (let ((bindings (pairlis (car (cdr fn)) (car args))))
      (dapply (print fn) (print bindings)))))

(define dapply
  (lambda (fn bindings)
    (bytecode
     ($LOAD bindings $LOOKUP
            $LOAD fn $LOOKUP
            $DAPPLY))))

;; codebuf is the output stream of instructions
(define eval
  (lambda (expr)
    (let ((code (compile expr)))
      (bytecode ($LOAD code $LOOKUP $EVAL)))))

(define codebuf-make
  (lambda ()
    (let ((buf (cons '() '())))
      (cons (cons 'start buf) (cons (cons 'next buf) '())))))

(define codebuf-start
  (lambda (buf)
    (cdr (assoc 'start buf))))

(define codebuf-next
  (lambda (buf)
    (cdr (assoc 'next buf))))

(define codebuf-append
  (lambda (buf el)
    (progn
      (let ((next (codebuf-next buf)))
        (setcar (codebuf-next buf) el)
        (setcdr next (cons $NOP '()))
        (setcdr (assoc 'next buf) (cdr next))))))

;; (define codebuf-append
;;   (lambda (buf el)
;;     (if (nilp (car (car buf)))
;;         (setcar buf el)
;;       (if (nilp (cdr buf))
;;           (setcdr buf (cons el ()))
;;         (codebuf-append (cdr buf) el)))))

(define *builtins* '())

(define compile
  (lambda (expr)
    (let ((buf (codebuf-make)))
      (progn
        (compile-internal buf expr)
        (codebuf-start buf)))))

(define compile-internal
  (lambda (buf expr)
    (if (consp expr)
        (compile-expr buf expr)
      (if (symbolp expr)
          (compile-lookup buf expr)
        (compile-const buf expr)))))

(define compile-emit codebuf-append)

(define compile-expr
  (lambda (buf expr)
    (if (compile-builtinp (car expr))
        (compile-builtin buf expr)
      (compile-raw-apply buf `(apply ,(car expr) ',(cdr expr))))))
      ;;(compile-raw-apply buf expr))))

(define compile-raw-apply
  (lambda (buf expr)
    (progn
      (compile-list buf (cdr expr))
      (compile-internal buf (car expr))
      (compile-emit buf $APPLY))))

(define compile-list
  (lambda (buf list)
    (progn
      (if (nilp list)
          (compile-const buf '())
        (progn
          (compile-list buf (cdr list))
          (compile-internal buf (car list))
          (compile-emit buf $CONS))))))

(define compile-cons
  (lambda (buf car cdr)
    (progn
      (compile-internal buf car)
      (compile-emit buf $CONS))))

(define compile-const
  (lambda (buf const)
    (progn
      (compile-emit buf $LOAD)
      (compile-emit buf const))))

(define compile-lookup
  (lambda (buf sym)
    (progn
      (compile-const buf sym)
      (compile-emit buf $LOOKUP))))

;; builtins
(define compile-builtinp
  (lambda (sym)
    (not (nilp (assoc sym *builtins*)))))

(define compile-builtin
  (lambda (buf expr)
    (let ((sym (car expr))
          (args (cdr expr)))
      ((assoc-default sym *builtins*) buf args))))

;; quote
(define builtin-quote
  (lambda (buf quoted)
    (compile-quote buf (car quoted))))

(define compile-quote
  (lambda (buf quoted)
    (if (consp quoted)
        (compile-quoted-list buf quoted)
      (compile-const buf quoted))))

(define compile-quoted-list
  (lambda (buf list)
    (progn
      (if (nilp list)
          (compile-const buf '())
        (progn
          (compile-quoted-list buf (cdr list))
          (compile-quote buf (car list))
          (compile-emit buf $CONS))))))

(add-to-alist '*builtins* 'quote builtin-quote)

;; backquote/unquote/splice
(define builtin-backquote
  (lambda (buf quoted)
    (compile-backquote buf (car quoted))))

(define compile-backquote
  (lambda (buf quoted)
    (if (consp quoted)
        (if (eq 'unquote (car quoted))
            (compile-internal buf (car (cdr quoted)))
          (compile-backquoted-list buf quoted))
      (compile-quote buf quoted))))

(define compile-backquoted-list
  (lambda (buf list)
    (progn
      (if (nilp list)
          (compile-const buf '())
        (if (and (consp list) (eq 'splice (car list)))
            (compile-internal buf (car (cdr list)))
          (progn
            (compile-backquoted-list buf (car (cdr list)))
            (compile-backquote buf (car list))
            (compile-emit buf $CONS)))))))

(add-to-alist '*builtins* 'backquote builtin-backquote)

;;
;; REPL
;;

(define toggle-trace
  (lambda () (set '*trace* (not *trace*))))

(define repl
  (lambda ()
    (progn
      (print (eval (read)))
      (repl))))

;(repl)
