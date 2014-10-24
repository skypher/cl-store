;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-store)

; special floats
(defun create-float-values (value &rest codes)
  "Returns a alist of special float to float code mappings."
  (sb-int:with-float-traps-masked (:overflow :invalid)
    (let ((neg-inf (expt value 3)))
      (mapcar 'cons
              (list (expt (abs value) 2)
                    neg-inf
                    (/ neg-inf neg-inf))
              codes))))

;; Custom structure storing

(defstore-cl-store (obj structure-object stream)
  (output-type-code +structure-object-code+ stream)
  (store-type-object obj stream))

(defrestore-cl-store (structure-object stream)
  (restore-type-object stream))
