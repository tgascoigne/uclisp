(define load-path '("lib"))
(define loaded-packages '())

(defun path-concat (&rest elements)
  (let (result)
    (dolist (elm elements result)
      (setq result (cond
                    (result (concat result "/" elm))
                    (t elm))))))

(defun package-path (package)
    (let (result)
    (setq package (concat package ".lisp"))
    (dolist (base load-path result)
      (unless result ; todo: we can't break out of dolist yet
        (let ((this-path (path-concat base package)))
          (when (file-exists-p this-path)
            (setq result this-path)))))))

(defun package-loaded-p (package)
  (list-contains-p loaded-packages package))

(defun require (package)
  (unless (package-loaded-p package)
    (let ((path (package-path package)))
      (if (eq nil path)
          (message "require: unable to locate package: %v" package))
      (load-file path)
      (add-to-list 'loaded-packages package))))

(deftests

  (test-expect 16
               (progn (require "test-package")
                      (square 4)))

  )
