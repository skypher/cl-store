;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-store)

;; Special float handling
(defun create-float-values (value &rest codes)
  (let ((neg-inf (expt value 3)))
    (mapcar 'cons
            (list (expt (abs value) 2)
                  neg-inf
                  (/ neg-inf neg-inf))
            codes)))

;; Custom structure storing from Alain Picard.
(defstore-cl-store (obj structure-object stream)
  (output-type-code +structure-object-code+ stream)
  (store-object (type-of obj) stream)
  (let ((slot-names (structure:structure-class-slot-names (class-of obj)))
        (count 0))
    (dolist (slot-name slot-names)
      (store-object slot-name stream)
      (store-object (slot-value obj slot-name) stream)
      (incf count))
    (store-object count stream)))

(defrestore-cl-store (structure-object stream)
  (restore-type-object stream))

;; EOF
