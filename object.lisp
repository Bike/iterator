(in-package #:iterator)

;;; Here, an "iterator" is an object encapsulating the iteration state.
;;; It's mostly intended as a kind of polite fiction rather than something to
;;; use at all - calls to map etc. with (iterator ...) forms will be compiler
;;; macroed into something non consing.
;;; Ditto accumulator.

(defclass iterator ()
  ((%object :initarg :object :reader object)
   (%iter :initarg :iter :accessor iter)
   (%limit :initarg :limit :reader limit)
   (%step :initarg :step :reader stepf)
   (%endp :initarg :endp :reader endpf)
   (%elt :initarg :elt :reader eltf)
   (%setelt :initarg :setelt :reader setelt)))

(defun iterator (object &rest kwargs &key &allow-other-keys)
  (multiple-value-bind (iter limit step endp elt setelt)
      (apply #'make-iterator object kwargs)
    (make-instance 'iterator
      :object object :iter iter :limit limit
      :step step :endp endp :elt elt :setelt setelt)))

(defun endp (iterator)
  (funcall (endpf iterator) (object iterator) (iter iterator) (limit iterator)))
(defun step (iterator)
  (setf (iter iterator)
        (funcall (stepf iterator) (object iterator) (iter iterator))))
(defun elt (iterator)
  (funcall (eltf iterator) (object iterator) (iter iterator)))
(defun (setf elt) (nv iterator)
  (funcall (setelt iterator) nv (object iterator) (iter iterator)))

(defclass accumulator ()
  ((%accum :initarg :accum :reader accum)
   (%idx :initarg :idx :accessor idx)
   (%add :initarg :add :reader add)
   (%finalize :initarg :finalize :reader finalizef)))

(defun accumulator (prototype &rest kwargs &key &allow-other-keys)
  (multiple-value-bind (accum idx add finalize)
      (apply #'make-accumulator prototype kwargs)
    (make-instance 'accumulator
      :accum accum :idx idx :add add :finalize finalize)))

(defun accumulate (new accumulator)
  (setf (idx accumulator)
        (funcall (add accumulator) new (accum accumulator) (idx accumulator))))
(defun finalize (accumulator)
  (funcall (finalizef accumulator) (accum accumulator) (idx accumulator)))
