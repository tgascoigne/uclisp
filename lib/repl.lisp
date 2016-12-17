;;
;; REPL
;;

(defun toggle-trace () (set '*trace* (not *trace*)))

(define *ltrace* nil)
(defun toggle-ltrace () (set '*ltrace* (not *ltrace*)))

(defun repl ()
  (progn
    (print (ucl-eval (read)))
    (repl)))

(repl)
