(in-package #:iterator)

(defun build-iteration (iterables body-builder result-builder)
  (let* ((element-accessors
           (loop repeat (length iterables) collect (gensym "ELT")))
         (vars
           (loop repeat (length iterables)
                 collect (list (gensym "ITERABLE")
                               (gensym "IT") (gensym "LIMIT") (gensym "STEP")
                               (gensym "ENDP") (gensym "ELT") (gensym "SELT"))))
         (main
           `(flet (,@(loop for acc in element-accessors
                           for (iterable iter _0 _1 _2 elt selt) in vars
                           collect `(,acc () (funcall ,elt ,iterable ,iter))
                           collect `((setf ,acc) (nv)
                                     (funcall ,selt nv ,iterable ,iter))))
              (declare (inline ,@(loop for acc in element-accessors
                                       collect acc
                                       collect `(setf ,acc)))
                       (dynamic-extent ,@(loop for acc in element-accessors
                                               collect `#',acc
                                               collect `#'(setf ,acc)))
                       (ignorable ,@(loop for acc in element-accessors
                                          collect `#',acc
                                          collect `#'(setf ,acc))))
              (do ()
                  ((or ,@(loop for (iterable iter limit _ endp) in vars
                               collect `(funcall ,endp ,iterable ,iter ,limit)))
                   ,(apply result-builder (mapcar #'first vars)))
                ,(apply body-builder element-accessors)
                (psetq ,@(loop for (iterable iter _ step) in vars
                               collect iter
                               collect `(funcall ,step ,iterable ,iter)))))))
    (labels ((aux (vars iterables)
               (if (null vars)
                   main
                   (destructuring-bind (iterable iter limit step endp elt selt)
                       (first vars)
                     (destructuring-bind (object &rest kwargs) (first iterables)
                       `(let ((,iterable ,object))
                          (multiple-value-bind
                                (,iter ,limit ,step ,endp ,elt ,selt)
                              (make-iterator ,iterable ,@kwargs)
                            ,(aux (rest vars) (rest iterables)))))))))
      (aux vars iterables))))

(defun iterator-form-p (form)
  (and (consp form)
       (eq (car form) 'iterator)
       (>= (length form) 2)
       (evenp (length form))))

(define-compiler-macro map-into (&whole whole result function &rest objects)
  (if (and (iterator-form-p result)
           (every #'iterator-form-p objects))
      (let ((functiong (gensym "FUNCTION")))
        `(let ((,functiong (function-designated ,function)))
           (declare (type function ,functiong))
           ,(build-iteration (mapcar #'rest (cons result objects))
                             (lambda (re &rest oes)
                               `(setf (,re) (funcall ,functiong
                                                     ,@(mapcar #'list oes))))
                             (lambda (ro &rest ign)
                               (declare (ignore ign))
                               ro))))
      whole))

(define-compiler-macro map-for-effect (&whole whole function &rest objects)
  (if (every #'iterator-form-p objects)
      (let ((functiong (gensym "FUNCTION")))
        `(let ((,functiong (function-designated ,function)))
           (declare (type function ,functiong))
           ,(build-iteration (mapcar #'rest objects)
                             (lambda (&rest oes)
                               `(funcall ,functiong ,@(mapcar #'list oes)))
                             (constantly nil))))
      whole))

(defun accumulator-form-p (form)
  (and (consp form)
       (eq (car form) 'accumulator)
       (>= (length form) 2)
       (evenp (length form))))

(define-compiler-macro map (&whole whole accumulator function &rest iterators)
  (if (and (accumulator-form-p accumulator)
           (every #'iterator-form-p iterators))
      (let ((functiong (gensym "FUNCTION"))
            (accumf (gensym "ACCUMULATE")))
        `(let ((,functiong (function-designated ,function)))
           (declare (type function ,functiong))
           (do-accumulator (,accumf (make-accumulator ,@(rest accumulator)))
             ,(build-iteration (mapcar #'rest iterators)
                               (lambda (&rest oes)
                                 `(,accumf (funcall ,functiong
                                                    ,@(mapcar #'list oes))))
                               (constantly nil)))))
      whole))

(define-compiler-macro concatenate (&whole whole accumulator &rest iterators)
  (if (and (accumulator-form-p accumulator)
           (every #'iterator-form-p iterators))
      (let ((accumf (gensym "ACCUMULATE")))
        `(do-accumulator (,accumf (make-accumulator ,@(rest accumulator)))
           ,@(loop for (ign obj . kwargs) in iterators
                   for osym = (gensym "OBJECT")
                   collect `(let ((,osym ,obj))
                              (do-iterator-elements
                                  (v ,osym (make-iterator ,osym ,@kwargs))
                                (,accumf v))))))
      whole))
