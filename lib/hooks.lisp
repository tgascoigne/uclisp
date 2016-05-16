;; an emacs-like hook system

(defmacro defhook (symbol)
  "Defined a new hook variable named `symbol'"
  `(define ,symbol '()))

(defun add-hook (hookvar function)
  "Adds `function' to the hook `hookvar'"
  (add-to-list hookvar function))

(defun run-hook (&rest hookvars)
  "Runs one or more `hookvars'. Hook functions are evaluated with no arguments."
  (dolist (hookvar hookvars)
    (run-hook-with-args hookvar)))

(defun run-hook-with-args (hookvar &rest args)
  "Runs all functions attached to `hookvar' with the given arguments"
  (dolist (fn (eval hookvar))
    (eval `(fn ,@args))))

(deftests

  (test-expect 1
               (let (x)
                 (defhook test-hook)
                 (add-hook 'test-hook (lambda (y) (setq x y)))
                 (run-hook-with-args 'test-hook 1)
                 x))

  (test-expect 3
               (let ((x 0))
                 (defhook test-hook)
                 (add-hook 'test-hook (lambda () (inc x)))
                 (add-hook 'test-hook (lambda () (inc x)))
                 (add-hook 'test-hook (lambda () (inc x)))
                 (run-hook 'test-hook)
                 x))

  )
