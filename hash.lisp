(in-package #:iterator)

;;; This is sketchy for multiple reasons, but mostly because it uses SBCL guts.
;;; These guts are not stable and this is not recommended for serious use; it's
;;; more just to demonstrate the possibility.
;;; The standard CL hash table iteration mechanisms are not suitable as far as
;;; I can tell. Something could be mashed together with with-hash-table-iterator
;;; but it would probably cons closures and such.

(defun hash-step (hash it)
  (loop with kv = (sb-impl::hash-table-pairs hash)
        for i from (+ it 2) by 2
        for key = (sb-impl::data-vector-ref kv (1- i))
        for value = (sb-impl::data-vector-ref kv i)
        until (not (or (sb-impl::empty-ht-slot-p key)
                       (sb-impl::empty-ht-slot-p value)))
        finally (return i)))

(defun hash-endp (hash it limit)
  (declare (ignore hash))
  (> it limit))

(defun hash-key (hash it)
  (sb-impl::data-vector-ref (sb-impl::hash-table-pairs hash) (1- it)))

(defun (setf hash-key) (hash it)
  (declare (ignore hash it))
  (error "What? No"))

(defun hash-value (hash it)
  (sb-impl::data-vector-ref (sb-impl::hash-table-pairs hash) it))

(defun (setf hash-value) (nv hash it)
  (let ((key (hash-key hash it)))
    (setf (gethash key hash) nv)))

(defmethod make-iterator ((hash hash-table) &key which)
  (let* ((kv-vector (sb-impl::hash-table-pairs hash))
         (limit (1+ (* 2 (sb-impl::kv-vector-high-water-mark kv-vector))))
         (index 3))
    (ecase which
      (:key
       (values index limit
               #'hash-step #'hash-endp
               #'hash-key #'(setf hash-key)))
      (:value
       (values index limit
               #'hash-step #'hash-endp
               #'hash-value #'(setf hash-value))))))
