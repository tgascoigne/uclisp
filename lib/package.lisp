(define load-path '("lib"))
(define loaded-packages '())

(defun path-concat (&rest elements)
  "Concatenate two or more file path elements"
  (let (result)
    (dolist (elm elements result)
      (setq result (cond
                    (result (concat result "/" elm))
                    (t elm))))))

(defun package-path (package)
  "Locates and returns the path to `package'. Searches inside each directory of `load-path' for a file named `package'.lisp

Returns nil if the package is not found"
  (let (result)
    (setq package (concat package ".lisp"))
    (dolist (base load-path result)
      (unless result ; todo: we can't break out of dolist yet
        (let ((this-path (path-concat base package)))
          (when (file-exists-p this-path)
            (setq result this-path)))))))

(defun package-loaded-p (package)
  "Evaluates to non-nil if `package' has been loaded"
  (list-contains-p loaded-packages package))

(defun require (package)
  "Loads `package' if it hasn't already been loaded"
  (unless (package-loaded-p package)
    (let ((path (package-path package)))
      (if (eq nil path)
          (message "require: unable to locate package: %v" package))
      (load-file path)
      (add-to-list 'loaded-packages package))))

(deftests

  (test-expect 16
               (progn
                 (add-to-list 'load-path "lib/test")
                 (require "test-package")
                 (test-foo 4)))

  )
