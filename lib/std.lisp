;;
;; Arithmetic
;;
(defun + (a b)
  (bytecode ($LOAD a $LOOKUP $LOAD b $LOOKUP $ADD)))

(defun - (a b)
  (bytecode ($LOAD a $LOOKUP $LOAD b $LOOKUP $SUB)))

(defun * (a b)
  (bytecode ($LOAD a $LOOKUP $LOAD b $LOOKUP $MUL)))

(defun / (a b)
  (bytecode ($LOAD a $LOOKUP $LOAD b $LOOKUP $DIV)))

(defun % (a b)
  (bytecode ($LOAD a $LOOKUP $LOAD b $LOOKUP $MOD)))

;;
;; Predicates
;;

(define t 1)

(defun and (c1 c2)
  (if c1
	  (if c2
		  t
		  nil)
	  nil))

(defun typeof (obj)
  (bytecode ($LOAD obj $LOOKUP $TYPE)))

(defun eq (a b)
  (bytecode ($LOAD a $LOOKUP $LOAD b $LOOKUP $EQUAL)))

(defun consp (obj)
  (eq (typeof obj) 'cons))

(defun nilp (obj)
  (eq obj nil))

(defun null (obj)
  (eq obj nil))

(defun symbolp (obj)
  (eq (typeof obj) 'symbol))

(defun opcodep (obj)
  (eq (typeof obj) 'opcode))

(defun not (x)
  (if x nil 1))

;;
;; I/O
;;

(defun read ()
  (bytecode ($READ)))

(defun print (el)
  (bytecode ($LOAD el $LOOKUP $PRINT $LOAD el $LOOKUP)))

;;
;; List manipulation
;;

(defun cons (car cdr)
  (bytecode ($LOAD cdr $LOOKUP $LOAD car $LOOKUP $CONS)))

(defun car (cons)
  (bytecode ($LOAD cons $LOOKUP $CAR)))

(defun cdr (cons)
  (bytecode ($LOAD cons $LOOKUP $CDR)))

(defun setcar (cell value)
  (bytecode ($LOAD value $LOOKUP $LOAD cell $LOOKUP $SETCAR)))

(defun setcdr (cell value)
  (bytecode ($LOAD value $LOOKUP $LOAD cell $LOOKUP $SETCDR)))

(defun list (&rest list)
  list)

(defun pairlis (keys values)
  (if (nilp keys)
	  nil
      (cons (cons (car keys) (car values))
            (pairlis (cdr keys) (cdr values)))))

(defun pairargs (keys values)
  (if (nilp keys)
	  nil
      (progn
		(if (eq (car keys) '&rest)
			(cons (cons (car (cdr keys)) values)
				  nil))
		(cons (cons (car keys) (car values))
			  (pairargs (cdr keys) (cdr values))))))

(defun ucl-map (fn sequence)
  (if (nilp sequence)
	  nil
      (cons (fn (car sequence)) (ucl-map fn (cdr sequence)))))

(defun append (list el)
  (if (nilp list)
	  (cons el '())
      (cons (car list) (append (cdr list) el))))

(defun prepend (el list)
  (cons el list))

(defun concat (a b)
  (if (nilp a)
	  b
      (cons (car a) (concat (cdr a) b))))

(defun symbol-value (sym)
  (bytecode ($LOAD sym $LOOKUP $LOOKUP)))

;;
;; Association lists
;;

(defun assoc (key list)
  (if (nilp list)
	  nil
      (if (eq (car (car list)) key)
          (car list))
      (assoc key (cdr list))))

(defun assoc-default (key list)
  (cdr (assoc key list)))

(defun add-to-alist (alist key value)
  (set alist (cons (cons key value) (symbol-value alist))))

(defun ucl-apply (fn &rest args)
  (progn
    (let ((bindings (pairargs (car (cdr fn)) args)))
	  (dapply fn bindings))))

(defun dapply (fn bindings)
  (bytecode ($LOAD bindings $LOOKUP
				   $LOAD fn $LOOKUP
				   $DAPPLY)))
