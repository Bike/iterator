(in-package #:iterator)

;;; Iterator protocol

(defgeneric make-iterator (object &key))
;;; helper
(defmacro with-iterator
    ((&whole vars &optional iterator limit step endp elt setelt)
     (object &rest kwargs &key &allow-other-keys)
     &body body)
  (declare (ignore iterator limit step endp elt setelt))
  (let* ((ignored nil)
         (vars
           (mapcar
            (lambda (v)
              (or v (let ((name (gensym))) (push name ignored) name)))
            vars)))
    `(multiple-value-bind (,@vars)
         (make-iterator ,object ,@kwargs)
       (declare (type function ,@(nthcdr 2 vars)) (ignore ,@ignored))
       ,@body)))

;;; Accumulator protocol

(defgeneric make-accumulator (object &key))
;;; helper
(defmacro with-accumulator ((&whole vars accum idx add finalize)
                            (object &rest kwargs &key &allow-other-keys)
                            &body body)
  (declare (ignore accum idx add finalize))
  (let* ((ignored nil)
         (vars
           (mapcar
            (lambda (v)
              (or v (let ((name (gensym))) (push name ignored) name)))
            vars)))
    `(multiple-value-bind (,@vars)
         (make-accumulator ,object ,@kwargs)
       (declare (type function ,@(nthcdr 2 vars)) (ignore ,@ignored))
       ,@body)))

;;; Error

(define-condition protocol-unimplemented (error)
  ((operation :initarg :operation
              :reader protocol-unimplemented-operation)))

(defun protocol-unimplemented (operation object)
  (declare (ignore object))
  (error 'protocol-unimplemented :operation operation))
