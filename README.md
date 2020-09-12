This is a simplification and generalization of Christophe Rhodes' extensible sequence protocol. It is generalized in the sense that it does not rely on the sequence having a stable indexing or finite size (although several operations are not sensible on infinitely large operands). It is simplified in the sense that, these things being irrelevant, there are fewer functions to define (but also an attendant loss of functionality).

## "Sequence" functions

There are reimplementations of many CL sequence functions and related. They are generic functions, but methods can only be defined as an optimization; behavior of iterables must be wholly specified by their methods on the protocol functions (see below).

Keyword arguments relating to their nature as sequences have been removed from the general signatures, e.g. _from-end_. They keep keyword arguments relating their function, e.g. _test-not_ or _count_. Other keyword arguments, including the sequence arguments, are passed on unmolested to _make-iterator_, which can do whatever with them.

Some functions have been skipped because they only make sense on ordered sequences: subseq, replace, reverse, mismatch, search, and sort. reduce is not implemented because its _from-end_ argument specifies both an iteration order and an associativity direction; instead there are _foldl_ and _foldr_. _position_ and etc. are defined, but may return silly results on non-sequences.

## n-arity "sequence" functions and iterator objects

The functions _map_, _map-into_, _concatenate_, and _merge_ are available but have a different interface. For example, _map_ takes an "accumulator" as its first argument rather than a type, and an "iterator" object instead of sequences. These are returned by the _accumulator_ and _iterator_ functions respectively. These two functions take an iterable object as their first argument, and keyword arguments valid to _make-iterator_ on that object for the rest, and return encapsulated objects for accumulator and iterator states, as described in the protocol below.

As an example, consider the following code: `(map (accumulator nil :from-end t) #'1+ (iterator (vector 4 5 6) :start 1))`. The accumulator has a list as its prototype argument, so `map` will return a list. `:from-end t` means it will accumulate results in reverse. `(iterator (vector 4 5 6) :start 1)` will iterate over 5 (because of the `:start`) and then 6 and then stop. So, first `(1+ 5) = 6` is accumulated, then `(1+ 6) = 7`; now the iterator has run out so `map` returns its result. Because of `:from-end t`, this is `(7 6)` rather than `(6 7)`.

Compiler macros on these functions will expand them into loops when passed arguments of the form `(iterator ...)` and `(accumulator ...`). This increases code size but means the encapsulation objects are never consed.

## Interface

Still working out what it should look like. There are _do-iterator_ and _do-iterator-elements_ exposed at the moment; given an object and an iterator form, they bind a setfable place and a variable (respectively) and then proceed sort of like _dolist_.

## Protocol

There are basically just two functions. Readers who are total dorks, i.e. know some Haskell or languages even nerdier than Haskell, may recognize them as limited forms of catamorphism and anamorphism respectively.

### make-iterator

Unlike extensible sequences, there is no required superclass for "iterable" objects. I have also, at least for now, skipped the "simple" protocol. As such, only one function needs to be defined for an object to be iterable:

_Generic Function_ **MAKE-ITERATOR**

**Syntax:** **make-iterator** _object &key => iterator, limit, step, endp, elt, (setf elt)_

**Arguments and Values:**

  * _iterator_ - whatever object is convenient.
  * _limit_ - ditto.
  * _step_ - a function of two arguments: the object and an iterator, which returns an iterator.
  * _endp_ - a function of three arguments: the object, an iterator, and the limit, which returns a generalized boolean.
  * _elt_ - a function of two arguments: the object and an iterator, which returns an element of the object.
  * _(setf elt)_ - a function of three arguments: a new value, the object, and an iterator, which returns the new value.

**Description:**

Methods on this function implement the iteration protocol. The state of iteration is represented by the returned iterator object; this object is only used as an argument to the other returned functions, so it can be whatever. The limit represents some kind of marker for detecting the iteration being complete.

An iteration proceeds as follows. First, _endp_ is called to see if the iteration is complete; if it is, calling any of the three functions on that iterator has undefined consequences (i.e. the implementor need not care). Then, _elt_ can be used to retrieve the "current" element of the object, whatever that means for the given iteration; similarly _(setf elt)_ can be used to change the current element. Finally, a new iterator object is returned from a call to the _step_ function and the process repeats.

**make-iterator** methods can take arbitrary keys. For example, a sequence may take _start_, _end_, and _from-end_ keyword arguments so that a subsequence and order may be specified without additional consing. A hash table might take an indicator for whether keys or values are the "elements" being considered.

The protocol is designed to minimize consing; thus the multitudinous return values rather than just returning closures.

### make-accumulator

Rather than the extensible sequences' _make-sequence-like_ and _adjust-sequence_ functions, which generally expect the object to have a size, here there is instead an "accumulation" protocol. An accumulator is an object in the process of being built up; for example for a list, you could use the common idiom of maintaining a head and then a tail, which is set to fresh conses in its cdr repeatedly.

_Generic Function_ **MAKE-ACCUMULATOR**

**Syntax:** **make-accumulator** _object &key => accumulator, index, accumulate, finalize_

**Arguments and Values:**

  * _accumulator_ - whatever object is convenient.
  * _index_ - ditto.
  * _accumulate_ - a function of three arguments: a new value, the accumulator, and the index, which returns a new index.
  * _finalize_ - a function of two arguments: the accumulator, and the index, which returns a completed object.

**Description:**

Defines the protocol for building up an object. _object_ serves as a prototype, i.e. an object of the same type will be constructed, but _object_'s elements are probably not relevant.. _accumulate_ is repeatedly called with new values to add to the object. Once all values have been added, _finalize_ is called and returns a completed object; after this, calling either function may result in undefined behavior.

_make-accumulator_ methods should be prepared to accept any keyword arguments that are allowed for the _make-iterator_ method on the same object, but they may be interpreted differently or simply ignored. For example, a _start_ keyword argument would not stop an accumulated sequence from starting at zero, but in concert with an _end_ argument may define the initial size for the accumulated sequence.
