
;; codebuf is the output stream of instructions
(defun codebuf-make ()
    (let ((buf (cons '() '())))
      (list (cons 'start buf) (cons 'next buf))))

(defun codebuf-start (buf)
    (cdr (assoc 'start buf)))

(defun codebuf-next (buf)
    (cdr (assoc 'next buf)))

(defun codebuf-append (buf el)
  (progn
    (let ((next (assoc 'next buf)))
      (setcar (cdr next) el)
      (let ((new-cell (cons $NOP '())))
        (setcdr (cdr next) new-cell)
        (setcdr next new-cell)))))

(defvar *builtins* '())

;; the actual compiler
(defun ucl-compile (expr)
    (let ((buf (codebuf-make)))
      (progn
        (compile-internal buf expr)
        (codebuf-start buf))))

(defun ucl-eval (expr)
    (let ((code (ucl-compile expr)))
      (list 'bytecode (list $LOAD code $LOOKUP $EVAL))))

(defun compile-internal (buf expr)
    (if (consp expr)
        (compile-expr buf expr)
		(if (and (symbolp expr)
				 (not (null expr))
				 (not (opcodep expr))) ; in sbcl, opcodes are symbols. make sure they're excluded from this
          (compile-lookup buf expr)
        (compile-const buf expr))))

(defun compile-emit (buf el)
  (progn
    (codebuf-append buf el)))

(defun compile-emit-instrs (buf instrs)
  (progn
    (if (not (null instrs))
        (progn
          (codebuf-append buf (car instrs))
          (compile-emit-instrs buf (cdr instrs))))))

(defun compile-expr (buf expr)
  (progn
	(if (compile-builtinp (car expr))
		(compile-builtin buf expr)
		;;          (compile-raw-apply buf 'ucl-apply expr))))
		(compile-raw-apply buf (car expr) (cdr expr)))))

(defun compile-apply (buf expr)
  (progn
	(let ((fn (car expr))
		  (args (car (cdr expr))))
	  (let ((bindings (pairargs (car (cdr fn)) args)))
		))))

(defun compile-raw-apply (buf fn args)
    (progn
      ;; compile arguments
      (compile-list buf args)
      ;; compile function
      (compile-internal buf fn)

      (compile-emit buf $APPLY)))

(defun compile-list (buf list)
    (progn
      (if (nilp list)
          (compile-const buf '())
        (progn
          (compile-list buf (cdr list))
          (compile-internal buf (car list))
          (compile-emit buf $CONS)))))

(defun compile-cons (buf car cdr)
    (progn
      (compile-internal buf car)
      (compile-emit buf $CONS)))

(defun compile-const (buf const)
    (progn
      (compile-emit buf $LOAD)
      (compile-emit buf const)))

(defun compile-lookup (buf sym)
    (progn
      (compile-const buf sym)
      (compile-emit buf $LOOKUP)))

(defun compile-lookupc (buf sym)
    (progn
      (compile-const buf sym)
      (compile-emit buf $LOOKUPC)))

;; builtins
(defun compile-builtinp (sym)
  (if (symbolp sym)
      (not (nilp (assoc sym *builtins*)))
    '()))

(defun compile-builtin (buf expr)
    (let ((sym (car expr))
          (args (cdr expr)))
      (funcall (assoc-default sym *builtins*) buf args)))

;; quote
(defun builtin-quote (buf quoted)
    (compile-quote buf (car quoted)))

(defun compile-quote (buf quoted)
    (if (consp quoted)
        (compile-quoted-list buf quoted)
      (compile-const buf quoted)))

(defun compile-quoted-list (buf list)
    (progn
      (if (nilp list)
          (compile-const buf '())
        (progn
          (compile-quoted-list buf (cdr list))
          (compile-quote buf (car list))
          (compile-emit buf $CONS)))))

;; backquote/unquote/splice
(defun builtin-backquote (buf quoted)
    (compile-backquote buf (car quoted)))

(defun compile-backquote (buf quoted)
    (if (consp quoted)
        (if (eq 'unquote (car quoted))
            (compile-internal buf (car (cdr quoted)))
          (compile-backquoted-list buf quoted))
      (compile-quote buf quoted)))

(defun compile-backquoted-list (buf list)
    (progn
      (if (nilp list)
          (compile-const buf '())
        (if (and (consp list) (eq 'splice (car list)))
            (compile-internal buf (car (cdr list)))
          (progn
            (compile-backquoted-list buf (car (cdr list)))
            (compile-backquote buf (car list))
            (compile-emit buf $CONS))))))

;; defun
(defun builtin-defun (buf args)
  (let ((symbol (car args))
        (argspec (car (cdr args)))
        (body (car (cdr (cdr args)))))
    (compile-internal buf
                      `(define ,symbol (lambda ,argspec ,body)))))

;; define
(defun builtin-define (buf args)
    (let ((symbol (car args))
          (value (car (cdr args))))
      (progn
        ;; Load the env list
        (compile-lookup buf '%env)
        (compile-emit buf $CAR)
        ;; Load the current env
        (compile-emit buf $CAR)
        ;; Create the new binding pair
        (compile-internal buf value)
        (compile-quote buf symbol)
        (compile-emit buf $CONS)
        ;; Cons it onto the current env
        (compile-emit buf $CONS)
        ;; Load the env list (again)
        (compile-lookup buf '%env)
        (compile-emit buf $CAR)
        ;; Update the current env to the newly updated list
        (compile-emit buf $SETCAR)
        ;; Lookup the new var for the return value
        (compile-internal buf symbol))))

;; set
(defun builtin-set (buf args)
    (let ((symbol (car args))
          (value (car (cdr args))))
      (progn
        ;; Compile the new value
        (compile-internal buf value)
        ;; Load the cell containing the var binding
        (compile-lookupc buf symbol)
        ;; Set the new value
        (compile-emit buf $SETCDR)
        ;; Lookup the new var for the return value
        (compile-internal buf symbol))))

;; progn
(defun builtin-progn (buf args)
    (progn
      ;; Compile all but the final expr, ignoring the result
      (builtin-progn-execn-1 buf args)
      ;; Compile the final expr, and return the result
      (compile-internal buf (builtin-progn-last-expr args))))

(defun builtin-progn-last-expr (exprs)
    (if (nilp (cdr exprs))
        (car exprs)
      (builtin-progn-last-expr (cdr exprs))))

(defun builtin-progn-execn-1 (buf exprs)
    (if (not (nilp (cdr exprs))) ;; the last expression will have cdr = nil
        (progn
          (compile-internal buf (car exprs))
          (compile-emit buf $DROP)
          (builtin-progn-execn-1 buf (cdr exprs)))))


;; lambda
(defun builtin-lambda (buf args)
    ;; fixme: shouldn't compile body, but we have to for compatibility with bootstrap
    ;;(compile-quote buf (prepend 'lambda args))))
    (let ((spec (car args))
          (body (car (cdr args))))
      (progn
        (compile-const buf ())
        (compile-const buf (append (ucl-compile body) (cons $RETURN nil)))
        (compile-emit buf $CONS)
        (compile-const buf spec)
        (compile-emit buf $CONS)
        (compile-const buf 'lambda)
        (compile-emit buf $CONS))))

;; if
(defun builtin-if (buf args)
  (let ((pred (car args))
        (then (car (cdr args)))
        (else (car (cdr (cdr args)))))
    (compile-internal buf pred)
    (compile-const buf (append (ucl-compile then) (cons $JOIN nil)))
    (compile-const buf (append (ucl-compile else) (cons $JOIN nil)))
    (compile-emit buf $SELECT)))

;; let
(defun builtin-let (buf args)
  (let ((bindings (car args))
        (body (car (cdr args))))
    (compile-internal buf `((lambda ,(ucl-map #'car bindings)
                              ,body)
                            ,@(ucl-map #'(lambda (el) (car (cdr el))) bindings)))))

;; let*
(defun builtin-let* (buf args)
  (let ((bindings (car args))
        (body (car (cdr args))))
	(compile-internal buf (let*-internal bindings body))))

(defun let*-internal (bindings body)
  (if (null bindings)
	  body
	  (let ((this-binding (car bindings))
			(other-bindings (cdr bindings)))
		`((lambda (,(car this-binding))
			,(let*-internal other-bindings body))
		  ,(car (cdr this-binding))))))

;; (let* ((foo 1)
;;        (bar (* foo 2)))
;;   (* bar 2))

;; ((lambda (foo)
;;    ((lambda (bar))
;;     (* bar 2)) (* foo 2))) 1

;; bytecode
(defun builtin-bytecode (buf args)
  (let ((instrs (car args)))
	(compile-emit-instrs buf instrs)))

;; function
;; for sbcl compat
;; translates #'fn -> (function fn) -> fn
(defun builtin-function (buf args)
  (let ((fn (car args)))
	fn))

(add-to-alist '*builtins* 'quote #'builtin-quote)
(add-to-alist '*builtins* 'backquote #'builtin-backquote)
(add-to-alist '*builtins* 'define #'builtin-define)
(add-to-alist '*builtins* 'defvar #'builtin-define)
(add-to-alist '*builtins* 'defun #'builtin-defun)
(add-to-alist '*builtins* 'set #'builtin-set)
(add-to-alist '*builtins* 'progn #'builtin-progn)
(add-to-alist '*builtins* 'lambda #'builtin-lambda)
(add-to-alist '*builtins* 'if #'builtin-if)
(add-to-alist '*builtins* 'let #'builtin-let)
(add-to-alist '*builtins* 'let* #'builtin-let*)
(add-to-alist '*builtins* 'bytecode #'builtin-bytecode)
(add-to-alist '*builtins* 'function #'builtin-function)
