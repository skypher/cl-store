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

(defun lw-old-structure-object-restore (new-instance length stream)
  ;; old format -- length is stored first
  (declare (optimize speed))
  (resolving-object (obj new-instance)
    (loop repeat length do
         (let ((slot-name (restore-object stream)))
           ;; slot-names are always symbols so we don't
           ;; have to worry about circularities
           (setting (slot-value obj slot-name) (restore-object stream))))))

(defun lw-normal-structure-object-restore (new-instance first-slot-name stream)
  ;; portable format -- length is stored at the end
  (declare (optimize speed))
  (resolving-object (obj new-instance)
    (setting (slot-value obj first-slot-name) (restore-object stream))
    (loop for count from 1 do
         (let ((slot-name (restore-object stream)))
           (etypecase slot-name
             (integer (assert (= count slot-name) (count slot-name)
                       "Number of slots restored does not match slots stored.")
                      (return))
             (symbol 
              ;; slot-names are always symbols so we don't
              ;; have to worry about circularities
              (setting (slot-value obj slot-name) (restore-object stream))))))))

(defrestore-cl-store (structure-object stream)
  (declare (optimize speed))
  (let* ((class (find-class (restore-object stream)))
         (new-instance (allocate-instance class))
         (slot-name-or-length (restore-object stream)))
    (etypecase slot-name-or-length
      (integer (unless (zerop slot-name-or-length)
                 (warn "Reading obsolete LispWorks structure-object format.")
                 (lw-old-structure-object-restore
                  new-instance slot-name-or-length stream)))
      (symbol (lw-normal-structure-object-restore
               new-instance slot-name-or-length stream)))
    new-instance))

;; EOF
