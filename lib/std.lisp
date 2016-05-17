;; Bare minimum stdlib to get us to the point of being able to load packages
;; load order is important here
(load-file "lib/decl.lisp")
(load-file "lib/test/framework.lisp")
(load-file "lib/decl-tests.lisp")
(load-file "lib/cond.lisp")
(load-file "lib/arithmetic.lisp")
(load-file "lib/iter.lisp")
(load-file "lib/listutil.lisp")
(load-file "lib/package.lisp")

;; Now we can begin loading packages..
(require "hooks")
(require "docstring")

(add-to-list 'load-path "examples")
;(require "cartesian")

(message "bah")
