(in-package #:iterator)

;; given a plist and a list of keywords, remove any instances of those keywords
;; and their corresponding values
(defun remove-kwargs (kwargs &rest kill)
  (loop for (key value) on kwargs by #'cddr
        unless (member key kill :test #'eq)
          collect key and collect value))

(declaim (inline function-designated))
(defun function-designated (function-designator)
  (etypecase function-designator
    (function function-designator)
    (symbol (fdefinition function-designator))))

(defmacro with-one-argument-test ((predicate key) &body body)
  (let ((predg (gensym "PREDICATE")) (keyg (gensym "KEY")))
    `(let ((,keyg ,key))
       (let ((,predg (function-designated ,predicate))
             (,keyg (if (null ,keyg) #'identity (function-designated ,keyg))))
         (declare (type function ,predg ,keyg))
         (flet ((satisfies-the-test (object)
                  (funcall ,predg (funcall ,keyg object))))
           (declare (dynamic-extent #'satisfies-the-test)
                    (inline satisfies-the-test))
           ,@body)))))

;;; NOTE: Might be technically incorrect in that it accepts an explicit NIL
;;; test as meaning EQL.
(defmacro with-two-argument-test ((test test-not key) &body body)
  (let ((test-notg (gensym "TEST-NOT")) (testg (gensym "TEST"))
        (keyg (gensym "KEY")))
    `(let ((,test-notg ,test-not) (,testg ,test) (,keyg ,key))
       (let ((,keyg (if (null ,keyg) #'identity (function-designated ,keyg)))
             (,testg
               (if ,test-notg
                   (if ,testg
                       (error ":test and :test-not both specified")
                       (complement (function-designated ,test-notg)))
                   (if ,testg
                       (function-designated ,testg)
                       #'eql))))
         (declare (type function ,keyg ,testg))
         (flet ((satisfies-the-test (object element)
                  (funcall ,testg object (funcall ,keyg element))))
           (declare (dynamic-extent #'satisfies-the-test)
                    (inline satisfies-the-test))
           ,@body)))))
