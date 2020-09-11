(defpackage #:iterator
  (:use #:cl)
  (:shadow #:count #:count-if #:count-if-not #:find #:find-if #:find-if-not
           #:position #:position-if #:position-if-not
           #:fill #:nsubstitute #:nsubstitute-if #:nsubstitute-if-not
           #:substitute #:substitute-if #:substitute-if-not
           #:delete #:delete-if #:delete-if-not
           #:remove #:remove-if #:remove-if-not
           #:delete-duplicates #:remove-duplicates
           #:every #:some #:notevery #:notany)
  (:export #:make-iterator #:make-accumulator
           #:with-iterator #:with-accumulator
           #:do-iterator #:do-iterator-elements)
  (:export #:count #:count-if #:count-if-not #:find #:find-if #:find-if-not
           #:position #:position-if #:position-if-not
           #:fill #:nsubstitute #:nsubstitute-if #:nsubstitute-if-not
           #:substitute #:substitute-if #:substitute-if-not
           #:delete #:delete-if #:delete-if-not
           #:remove #:remove-if #:remove-if-not
           #:delete-duplicates #:remove-duplicates
           #:every #:some #:notevery #:notany)
  (:export #:foldr #:foldl #:map-like/1))
