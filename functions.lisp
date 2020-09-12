(in-package #:iterator)

(defgeneric count (item object &key test test-not key))
(defgeneric count-if (predicate object &key key))
(defgeneric count-if-not (predicate object &key key))
(defgeneric find (item object &key test test-not key))
(defgeneric find-if (predicate object &key key))
(defgeneric find-if-not (predicate object &key key))
(defgeneric fill (object item &key))
(defgeneric nsubstitute (new old object &key test test-not count key))
(defgeneric nsubstitute-if (new predicate object &key count key))
(defgeneric nsubstitute-if-not (new predicate object &key count key))
(defgeneric substitute (new old object &key test test-not count key))
(defgeneric substitute-if (new predicate object &key count key))
(defgeneric substitute-if-not (new predicate object &key count key))
(defgeneric delete (item object &key test test-not count key))
(defgeneric delete-if (predicate object &key count key))
(defgeneric delete-if-not (predicate object &key count key))
(defgeneric remove (item object &key test test-not count key))
(defgeneric remove-if (predicate object &key count key))
(defgeneric remove-if-not (predicate object &key count key))
(defgeneric delete-duplicates (object &key test test-not key))
(defgeneric remove-duplicates (object &key test test-not key))
;; subseq, replace, reverse, mismatch, search, sort
;; since :from-end to reduce specifies associativity direction as well, it's
;; not suitable generically. Instead,
(defgeneric foldl (function object initial-value &key key))
(defgeneric foldr (function object initial-value &key key))
;;;
(defgeneric every (predicate object &key))
(defgeneric some (predicate object &key))
(defgeneric notevery (predicate object &key))
(defgeneric notany (predicate object &key))
;;; These three give inconsistent results if the iteration doesn't have a
;;; consistent order, but they can still be defined.
(defgeneric position (item sequence &key test test-not key))
(defgeneric position-if (predicate sequence &key key))
(defgeneric position-if-not (predicate sequence &key key))
;;;
(defgeneric map-like/1 (prototype function object &key))
