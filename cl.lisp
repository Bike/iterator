(in-package #:iterator)

;;; List

(defun list-step-forward (list it)
  (declare (ignore list))
  (cdr it))

(defun list-endp-forward (list it limit)
  (declare (ignore list))
  (eq it limit))

(defun list-elt (list it)
  (declare (ignore list))
  (car it))

(defun (setf list-elt) (new-value list it)
  (declare (ignore list))
  (setf (car it) new-value))

(defmethod make-iterator ((object list) &key (start 0) end from-end)
  (when from-end (error "FIXME: Not supported yet, sorry!"))
  (let ((lstart (nthcdr start object)))
    (values lstart (if end (nthcdr (- end start) lstart) nil)
            #'list-step-forward #'list-endp-forward
            #'list-elt #'(setf list-elt))))

(defun list-accumulate-forward (new-value accum idx)
  (declare (ignore accum))
  (setf (cdr idx) (list new-value)))

(defun list-finalize-forward (accum idx)
  (declare (ignore idx))
  (cdr accum))

(defmethod make-accumulator ((object list) &key (start 0) end from-end)
  (declare (ignore start end))
  (when from-end (error "FIXME: Not supported yet, sorry!"))
  (let ((accum (list nil)))
    (values accum accum #'list-accumulate-forward #'list-finalize-forward)))

 ;;; Vector

(defun vector-step-forward (vector it)
  (declare (ignore vector) (type (integer 0 (#.array-dimension-limit)) it))
  (incf it))

(defun vector-step-back (vector it)
  (declare (ignore vector) (type (integer -1 (#.array-dimension-limit)) it))
  (decf it))

(defun vector-endp (vector it limit)
  (declare (ignore vector)
           (type (integer -1 (#.array-dimension-limit)) it limit))
  (= it limit))

(defun vector-elt (vector it)
  (declare (type vector vector)
           (type (integer 0 (#.array-dimension-limit)) it))
  (aref vector it))

(defun (setf vector-elt) (new-value vector it)
  (declare (type vector vector)
           (type (integer 0 (#.array-dimension-limit)) it))
  (setf (aref vector it) new-value))

(defmethod make-iterator ((object vector)
                          &key (start 0) (end (length object)) from-end)
  (values (if from-end (1- end) start) (if from-end (1- start) end)
          (if from-end #'vector-step-back #'vector-step-forward)
          #'vector-endp #'vector-elt #'(setf vector-elt)))

(defun vector-accumulate-forward (new-value accum idx)
  (setf (aref accum idx) new-value)
  (1+ idx))

(defun vector-accumulate-back (new-value accum idx)
  (setf (aref accum idx) new-value)
  (1- idx))

(defun vector-finalize-forward (accum idx)
  (subseq accum 0 idx))

(defun vector-finalize-back (accum idx)
  (subseq accum idx))

(defmethod make-accumulator ((object vector)
                             &key (start 0) (end (length object)) from-end)
  (values (make-array (- end start)
                      :element-type (array-element-type object))
          (if from-end (- end start 1) 0)
          (if from-end #'vector-accumulate-back #'vector-accumulate-forward)
          (if from-end #'vector-finalize-back #'vector-finalize-forward)))
