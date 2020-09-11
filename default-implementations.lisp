(in-package #:iterator)

(defmethod count (item object &rest kwargs &key test test-not key)
  (let ((counter 0)
        (new-kwargs (remove-kwargs kwargs :test :test-not :key)))
    (with-two-argument-test (test test-not key)
      (do-iterator-elements (e object
                               (apply #'make-iterator object new-kwargs)
                               counter)
        (when (satisfies-the-test item e) (incf counter))))))

(defmethod count-if (pred object &rest kwargs &key key)
  (let ((counter 0)
        (new-kwargs (remove-kwargs kwargs :key)))
    (with-one-argument-test (pred key)
      (do-iterator-elements (e object
                               (apply #'make-iterator object new-kwargs)
                               counter)
        (when (satisfies-the-test e) (incf counter))))))

(defmethod count-if-not (pred object &rest kwargs &key key)
  (let ((counter 0)
        (new-kwargs (remove-kwargs kwargs :key)))
    (with-one-argument-test (pred key)
      (do-iterator-elements (e object
                               (apply #'make-iterator object new-kwargs)
                               counter)
        (unless (satisfies-the-test e) (incf counter))))))

(defmethod find (item object &rest kwargs &key test test-not key)
  (let ((new-kwargs (remove-kwargs kwargs :test :test-not :key)))
    (with-two-argument-test (test test-not key)
      (do-iterator-elements (e object
                               (apply #'make-iterator object new-kwargs) nil)
        (when (satisfies-the-test item e) (return e))))))

(defmethod find-if (pred object &rest kwargs &key key)
  (let ((new-kwargs (remove-kwargs kwargs :key)))
    (with-one-argument-test (pred key)
      (do-iterator-elements (e object
                               (apply #'make-iterator object new-kwargs) nil)
        (when (satisfies-the-test e) (return e))))))

(defmethod find-if-not (pred object &rest kwargs &key key)
  (let ((new-kwargs (remove-kwargs kwargs :key)))
    (with-one-argument-test (pred key)
      (do-iterator-elements (e object
                               (apply #'make-iterator object new-kwargs) nil)
        (unless (satisfies-the-test e) (return e))))))

(defmethod position (item object &rest kwargs &key test test-not key)
  (let ((new-kwargs (remove-kwargs kwargs :key))
        (pos 0))
    (with-two-argument-test (test test-not key)
      (do-iterator-elements (e object
                               (apply #'make-iterator object new-kwargs) nil)
        (when (satisfies-the-test item e) (return pos))
        (incf pos)))))

(defmethod position-if (pred object &rest kwargs &key key)
  (let ((new-kwargs (remove-kwargs kwargs :key))
        (pos 0))
    (with-one-argument-test (pred key)
      (do-iterator-elements (e object
                               (apply #'make-iterator object new-kwargs))
        (when (satisfies-the-test e) (return pos))
        (incf pos)))))

(defmethod position-if-not (pred object &rest kwargs &key key)
  (let ((new-kwargs (remove-kwargs kwargs :key))
        (pos 0))
    (with-one-argument-test (pred key)
      (do-iterator-elements (e object
                               (apply #'make-iterator object new-kwargs))
        (unless (satisfies-the-test e) (return pos))
        (incf pos)))))

(defmethod fill (object item &rest kwargs &key)
  (do-iterator (eltf object (apply #'make-iterator object kwargs) object)
    (setf (eltf) item)))

(defmethod nsubstitute (new old object
                        &rest kwargs &key test test-not count key)
  (let ((counter 0)
        (new-kwargs (remove-kwargs kwargs :test :test-not :count :key)))
    (with-two-argument-test (test test-not key)
      (do-iterator (eltf object (apply #'make-iterator object new-kwargs)
                         object)
        (when (and count (= counter count)) (return object))
        (when (satisfies-the-test old (eltf))
          (incf counter)
          (setf (eltf) new))))))

(defmethod nsubstitute-if (new pred object &rest kwargs &key count key)
  (let ((counter 0)
        (new-kwargs (remove-kwargs kwargs :count :key)))
    (with-one-argument-test (pred key)
      (do-iterator (eltf object (apply #'make-iterator object new-kwargs)
                         object)
        (when (and count (= counter count)) (return object))
        (when (satisfies-the-test (eltf))
          (incf counter)
          (setf (eltf) new))))))

(defmethod nsubstitute-if-not
    (new pred object &rest kwargs &key count key)
  (let ((counter 0)
        (new-kwargs (remove-kwargs kwargs :count :key)))
    (with-one-argument-test (pred key)
      (do-iterator (eltf object (apply #'make-iterator object new-kwargs)
                         object)
        (when (and count (= counter count)) (return object))
        (unless (satisfies-the-test (eltf))
          (incf counter)
          (setf (eltf) new))))))

(defmethod substitute (new old object
                       &rest kwargs &key test test-not count key)
  (let ((counter 0)
        (new-kwargs (remove-kwargs kwargs :test :test-not :count :key)))
    (do-accumulator (accum (apply #'make-accumulator object new-kwargs))
      (with-two-argument-test (test test-not key)
        (do-iterator-elements (e object
                                 (apply #'make-iterator object new-kwargs))
          (when (and count (= counter count)) (return))
          (incf counter)
          (accum (if (satisfies-the-test old e) new e)))))))

(defmethod substitute-if (new pred object &rest kwargs &key count key)
  (let ((counter 0)
        (new-kwargs (remove-kwargs kwargs :count :key)))
    (do-accumulator (accum (apply #'make-accumulator object new-kwargs))
      (with-one-argument-test (pred key)
        (do-iterator-elements (e object
                                 (apply #'make-iterator object new-kwargs))
          (when (and count (= counter count)) (return))
          (incf counter)
          (accum (if (satisfies-the-test e) new e)))))))

(defmethod substitute-if-not (new pred object &rest kwargs &key count key)
  (let ((counter 0)
        (new-kwargs (remove-kwargs kwargs :count :key)))
    (do-accumulator (accum (apply #'make-accumulator object new-kwargs))
      (with-one-argument-test (pred key)
        (do-iterator-elements (e object
                                 (apply #'make-iterator object new-kwargs))
          (when (and count (= counter count)) (return))
          (incf counter)
          (accum (if (satisfies-the-test e) e new)))))))

;;; Need some more thinking about an adjust-sequence/whatever equiv. here.
(defmethod delete (item object &rest kwargs)
  (apply #'remove item object kwargs))
(defmethod delete-if (predicate object &rest kwargs)
  (apply #'remove-if predicate object kwargs))
(defmethod delete-if-not (predicate object &rest kwargs)
  (apply #'remove-if-not predicate object kwargs))

(defmethod remove (item object &rest kwargs &key test test-not count key)
  (let ((counter 0)
        (new-kwargs (remove-kwargs kwargs :test :test :test-not :count)))
    (with-two-argument-test (test test-not key)
      (do-accumulator (accum (apply #'make-accumulator object new-kwargs))
        (do-iterator-elements (e object
                                 (apply #'make-iterator object new-kwargs))
        (when (or (and count (>= counter count))
                  (not (satisfies-the-test item e)))
          (incf counter)
          (accum e)))))))

(defmethod remove-if (pred object &rest kwargs &key count key)
  (let ((counter 0)
        (new-kwargs (remove-kwargs kwargs :count :key)))
    (with-one-argument-test (pred key)
      (do-accumulator (accum (apply #'make-accumulator object new-kwargs))
        (do-iterator-elements (e object
                                 (apply #'make-iterator object new-kwargs))
          (when (or (and count (>= counter count))
                    (not (satisfies-the-test e)))
            (incf counter)
            (accum e)))))))

(defmethod remove-if-not (pred object &rest kwargs &key count key)
  (let ((counter 0)
        (new-kwargs (remove-kwargs kwargs :count :key)))
    (with-one-argument-test (pred key)
      (do-accumulator (accum (apply #'make-accumulator object new-kwargs))
        (do-iterator-elements (e object
                                 (apply #'make-iterator object new-kwargs))
          (when (or (and count (>= counter count))
                    (satisfies-the-test e))
            (incf counter)
            (accum e)))))))

;;;

(defmethod foldl (function object initial-value
                  &rest kwargs &key (key #'identity))
  (let ((new-kwargs (remove-kwargs kwargs :key)))
    (do-iterator-elements (e object
                             (apply #'make-iterator object new-kwargs)
                             initial-value)
      (setf initial-value (funcall function initial-value (funcall key e))))))

(defmethod foldr (function object initial-value
                  &rest kwargs &key (key #'identity))
  (let ((new-kwargs (remove-kwargs kwargs :key)))
    (do-iterator-elements (e object
                             (apply #'make-iterator object new-kwargs)
                             initial-value)
      (setf initial-value (funcall function (funcall key e) initial-value)))))

;;;

(defmethod every (predicate object &rest kwargs)
  (do-iterator-elements (e object (apply #'make-iterator object kwargs) t)
    (unless (funcall predicate e) (return nil))))
(defmethod some (predicate object &rest kwargs)
  (do-iterator-elements (e object (apply #'make-iterator object kwargs) nil)
    (when (funcall predicate e) (return t))))
(defmethod notevery (predicate object &rest kwargs)
  (do-iterator-elements (e object (apply #'make-iterator object kwargs) nil)
    (unless (funcall predicate e) (return t))))
(defmethod notany (predicate object &rest kwargs)
  (do-iterator-elements (e object (apply #'make-iterator object kwargs) t)
    (when (funcall predicate e) (return nil))))

;;;

(defmethod map-like/1 (prototype function object &rest kwargs)
  (do-accumulator (accum (apply #'make-accumulator prototype kwargs))
    (do-iterator-elements (e object (apply #'make-iterator object kwargs))
      (accum (funcall function e)))))
