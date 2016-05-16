;; an emacs-like hook system

(defmacro defhook (symbol)
  `(define ,symbol '()))

(defun add-hook (hook function)
  (add-to-list hook function))

(defun run-hook (&rest hookvars)
  (dolist (hookvar hookvars)
    (run-hook-with-args hookvar)))

(defun run-hook-with-args (hookvar &rest args)
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
