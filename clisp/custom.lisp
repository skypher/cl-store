(in-package :cl-store)

(defun cl-function-p (fn)
  (eql #.(find-package :cl)
       (symbol-package (nth-value 2 (function-lambda-expression fn)))))

(defstore-cl-store (obj function stream)
  (if (cl-function-p obj)
      (dump-builtin-function obj stream)
      (dump-closure obj stream)))

(defun dump-builtin-function (obj stream)
  (output-type-code +built-in-function-code+ stream)
  (store-object (get-function-name obj) stream))

(defun dump-closure (obj stream)
  (output-type-code +function-code+ stream)
  (flet ((so (object)
           (store-object object stream)))
    (mapc #'so (multiple-value-list (function-lambda-expression obj)))
    (if (compiled-function-p obj)
        (flet ((es (func) ;; extract-and-store
                 (store-object (funcall func obj) stream)))
          (mapc #'es
                (list #'sys::closure-consts
                      #'sys::closure-codevec
                      #'sys::closure-documentation
                      #'sys::closure-lambda-list)))
        (dotimes (i 4) (so nil)))))

(defrestore-cl-store (function stream)
  (flet ((ro () (restore-object stream)))
    (let ((lambda-exp (ro))
          (closure-p (ro))
          (name (ro))
          (consts (ro))
          (codevec (ro))
          (doc (ro))
          (lambda-list (ro)))
      (declare (ignore closure-p))
      (if codevec ;; compiled
          ;; TODO What is a suitable default seclass? Currently ()
          (funcall (find-symbol "%MAKE-CLOSURE" "SYS") name codevec consts () lambda-list doc)
          ;; TODO Any functions to do this programmatically?  How to
          ;; store/restore dynamic, lexical, etc environment.
          (eval lambda-exp)))))

(defrestore-cl-store (built-in-function stream)
  (fdefinition (restore-object stream)))

;; EOF
