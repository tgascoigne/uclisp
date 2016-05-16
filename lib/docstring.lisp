(defun docstring-set (symbol docstring)
  "Assigns the docstring `docstring' to the symbol `symbol'"
  (set 'docstring-alist (append `((,symbol ,docstring)) docstring-alist)))

(defun documentation (symbol)
  "Returns the documentation string for the symbol `symbol'"
  (let (docstring)
    (dolist (doc docstring-alist docstring)
      (when (eq (nth 0 doc) symbol)
        (setq docstring (nth 1 doc))))))
