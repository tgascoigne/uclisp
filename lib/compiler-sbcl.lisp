(setf (readtable-case *readtable*) :invert)

(defmacro setcar (el val)
  `(setf (car ,el) ,val))

(defmacro setcdr (el val)
  `(setf (cdr ,el) ,val))

(defun add-to-alist (alist key value)
  (set alist (cons (cons key value) (eval alist))))

(defun nilp (val)
  (null val))

(defmacro assoc-default (k alist)
  `(cdr (assoc ,k ,alist)))

(defun fasl-print (bc)
  (princ-to-string `(bytecode ,bc)))

(defun ucl-map (fn list)
  (map 'list fn list))

(defmacro define-opcode (opcode)
  `(defvar ,opcode ',opcode))

(defvar ucl-opcodes
  (list '$NOP
    '$LOAD
    '$LOOKUP
    '$LOOKUPC
    '$CONS
    '$CAR
    '$CDR
    '$SETCAR
    '$SETCDR
    '$TYPE
    '$APPLY
    '$DAPPLY
    '$RETURN
    '$EVAL
    '$DROP
    '$DUP
    '$SELECT
    '$JOIN
    '$EQUAL
    '$ADD
    '$SUB
    '$MUL
    '$DIV
    '$MOD
    '$READ
    '$PRINT
    '$END))

(defun opcodep (sym)
  (not (null (find sym ucl-opcodes))))

(define-opcode $NOP)
(define-opcode $LOAD)
(define-opcode $LOOKUP)
(define-opcode $LOOKUPC)
(define-opcode $CONS)
(define-opcode $CAR)
(define-opcode $CDR)
(define-opcode $SETCAR)
(define-opcode $SETCDR)
(define-opcode $TYPE)
(define-opcode $APPLY)
(define-opcode $DAPPLY)
(define-opcode $RETURN)
(define-opcode $EVAL)
(define-opcode $DROP)
(define-opcode $DUP)
(define-opcode $SELECT)
(define-opcode $JOIN)
(define-opcode $EQUAL)
(define-opcode $ADD)
(define-opcode $SUB)
(define-opcode $MUL)
(define-opcode $DIV)
(define-opcode $MOD)
(define-opcode $READ)
(define-opcode $PRINT)
(define-opcode $END)

(load "compiler.lisp")

;(loop (fasl-print (ucl-compile (read))))
