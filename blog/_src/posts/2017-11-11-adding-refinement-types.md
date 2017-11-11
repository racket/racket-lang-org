    Title: Refinement Types in Typed Racket
    Date: 2017-11-11T09:00:00
    Tags:

*posted by Andrew Kent*

With the Racket v6.11 release, Typed Racket has begun to support
basic refinement and dependent function types.
This post gives an overview for working with these types while writing
some simple vector operations.

Currently these types are documented under Typed Racket's
[experimental
features](https://docs.racket-lang.org/ts-reference/Experimental_Features.html). They
are experimental in the sense that they are relatively new. We look
forward to improving their usefulness by incorporating user feedback.

**Note:** If you wish to experiment with refinement and/or
dependent function types in the near future, you should use a [snapshot
build](https://pre.racket-lang.org/installers/), as it will have
additional features and bug fixes from user feedback. Some of the
examples in this post will require a build from early November 2017
or later.

<!-- more -->


# Refinement Types

Let us examine the type of `vector-ref` in Typed Racket:

```racket
(All (a) (-> (Vectorof a) Integer a))
```

It states that the index (the second argument) must be an `Integer`,
i.e. any whole-number value in the exclusive range (-∞ , ∞). This
description, however, is somewhat coarse: we obviously cannot use
_any_ integer. To avoid a runtime error when indexing into a vector
`v`, we must use an integer in the range [0, `(vector-length
v)`). With a refinement type we can precisely describe this subset of
the integers:

```racket
(Refine [i : Integer] (<= 0 i (- (vector-length v) 1)))
```

This refinement type describes values `i` of type `Integer` such that
`(<= 0 i (- (vector-length v) 1))` produces `#true`. The documentation
for [Refine](http://docs.racket-lang.org/search/index.html?q=Refine)
describes precisely what propositions can be used in
refinements. Today it includes logical statements about the types of
terms, standard logical combinators (e.g. and, or), and linear integer
constraints.

# Dependent function types

By combining refinement types with dependent function types
(i.e. functions whose domain and range may depend on argument and/or
program values), we can now give a more precise function type for many
basic vector operations, e.g.:

```racket
(: safe-vector-ref 
   (All (A) (-> ([v : (Vectorof A)]
                 [n : (v) (Refine [i : Integer]
                            (<= 0 i (- (vector-length v) 1)))])
                A)))
(define safe-vector-ref vector-ref)
```

Here we have stated that `safe-vector-ref`'s second argument (`n`)
depends on its first argument (`v`) and that the type of `n` is an
`Integer` which is a valid index for `v`.

If we try and use `safe-vector-ref` with a vector and index which
Typed Racket cannot prove satisfy the respective types:

```racket
(safe-vector-ref (vector 0 1) 2)
```

Typed Racket will report an error while type checking:

```
Type Checker: type mismatch
  expected: (Refine
             (i : Integer)
             (and (<= i (- (vector-length v) 1)) (<= 0 v)))
  given: (Refine (z : Positive-Byte) (= 2 z)) in: 2
```

## Preconditions

Sometimes the refinements used in a dependent function type can also
be written conveniently as a "precondition", e.g.:

```racket
(: safe-vector-ref
   (All (A) (-> ([v : (Vectorof A)]
                 [n : Integer])
                #:pre (v n)
                (<= 0 n (- (vector-length v) 1))
                A)))
(define safe-vector-ref vector-ref)
```

This function type is equivalent to the previous version, but the
refining proposition that requires that `n` be a valid index for `v`
now references `n` directly and has been written after the
list of arguments:

```racket
#:pre (v n) (<= 0 n (- (vector-length v) 1))
```

We can read the precondition specification above as follows: the
precondition for the function depends on arguments `v` and `n` and it
requires that Typed Racket can prove `(<= 0 n (- (vector-length v)
1))` in order for a call to be well-typed.

Typed Racket may be able to provide more detailed feedback when the
arguments are of the correct type but the precondition cannot be
proved:

```
Type Checker: could not apply function;
 unable to prove precondition
  precondition: (<= 2 (- (vector-length v) 1)) 
  in: (safe-vector-ref (vector 0 1) 2)
```

Here, Typed Racket first performed type inference and confirmed that
the arguments `(vector 0 1)` and `2` had acceptable types before
discovering the precondition was unprovable at the call site.

We believe the error messages produced when using preconditions may be
more informative in general. Your mileage may vary.

# Programming with refinement types

_Note: the below examples will work on Racket v6.11 with one
additional snippet of [code](#makevectortype) or on any Racket build
more recent than November 11 2017._

When programming with refinement types, use the `#:with-refinements`
option on the `#lang` line to tell Typed Racket to track additional
logical information about program terms:

```racket
;; safe-vector-ops.rkt
#lang typed/racket/base #:with-refinements
```

In particular, this will tell Typed Racket to remember that the vector
produced by `(make-vector n ...)` has length `n`, which will be
essential for typing our vector operations.

Now we can also declare more detailed types for `vector-set!` and
`vector-ref` since these more specific types are subtypes of their
more permissive standard types. If we're feeling adventurous, we may
use the `unsafe-` variants from `racket/unsafe/ops` that do not check
their bounds, since Typed Racket proves they are never used
with invalid indices:

```racket
(require (only-in racket/unsafe/ops
                  unsafe-vector-ref
                  unsafe-vector-set!))

(: safe-vector-ref
   (All (A) (-> ([v : (Vectorof A)]
                 [n : Natural])
                #:pre (v n)
                (< n (vector-length v))
                A)))
(define safe-vector-ref unsafe-vector-ref)

(: safe-vector-set!
   (All (A) (-> ([v : (Vectorof A)]
                 [n : Natural]
                 [a : A])
                #:pre (v n)
                (< n (vector-length v))
                Void)))
(define safe-vector-set! unsafe-vector-set!)
```

Now that we have these foundational operations in place
(i.e. `make-vector`, `safe-vector-ref`, and `safe-vector-set!`) ,
let's see if we can define some of the [vector
operations](https://docs.racket-lang.org/reference/vectors.html) that
are provided by `racket/base` and `racket/vector`, but with more
precise types!

## `build-vector`

Let's look at the documentation for `build-vector`:

```
(build-vector n proc) → vector?
  n : exact-nonnegative-integer?
  proc : (exact-nonnegative-integer? . -> . any/c)

Creates a vector of n elements by applying proc to the
integers from 0 to (sub1 n) in order. If vec is the 
resulting vector, then (vector-ref vec i) is the value 
produced by (proc i).

Example:
> (build-vector 5 add1)
'#(1 2 3 4 5)
```

First let's consider its type signature. It seems like there are two
places where we could use refinement types to create a more precise
type-based specification:

1. the `proc` procedure will always be given an integer in the range
[0, `n`), and

2. the length of the returned vector is equal to `n`.

We can encode both of these directly in the type of `build-vector`:

```racket
(: build-vector
   (All (A) (-> ([n : Natural]
                 [proc : (n)
                   (-> (Refine [i : Natural] (< i n))
                       A)])
                (Refine [v : (Vectorof A)]
                  (= n (vector-length v))))))
```

With the type signature in place, we can proceed to define the
function:

```racket
(define (build-vector n proc)
  (cond
    [(> n 0)
     (define init-val (proc 0))
     (define vec (make-vector n init-val))
     (let loop! ([i : Natural 1])
       (when (< i n)
         (safe-vector-set! vec i (proc i))
         (loop! (add1 i))))
     vec]
    [else (vector)]))
```

There are a few things to note about the definition:

a. In order to make a `(Vectorof A)`, we need some value of type `A`
to initialize the vector with (i.e. the second arg to
`make-vector`). Because of this, we first check if `n` is greater than
0. If it is greater than 0, we can compute `(proc 0)` and use that to
initialize the vector we create, otherwise we simply return `(vector)`
(i.e. an empty vector) which is "vacuously" a vector of `A` for any
`A`.

b. The recursive loop we use to populate indices 1 through `(sub1 n)`,
which we named `loop!`, requires a type annotation `Natural` on its
argument `i` so that its being greater than or equal to 0 is known in
the body of the recursive function. That, combined with the test `(< i
n)`, allows us to successfully use `safe-vector-set!` to update the
contents at `i`. (Note: if Typed Racket had smarter type inference,
this annotation may not be necessary.)


## `vector-map`

`vector-map` takes a vector `vec` and a function `proc` and produces a
new vector `vec*` where `(vector-ref vec* i)` equals `(proc
(vector-ref vec i))` for each valid index `i`:

```racket
> (vector-map add1 (vector 0 1 2 3))
'#(1 2 3 4)
```

Because we now have a version of `build-vector` that uses refinement
types to more precisely describe its specification (see previous
section), we can implement `vector-map` entirely in terms of
`build-vector`:

```racket
(: vector-map
   (All (A B) (-> ([proc : (-> A B)]
                   [vec : (Vectorof A)])
                  (Refine [v : (Vectorof B)]
                    (= (vector-length vec)
                       (vector-length v))))))
(define (vector-map proc vec)
  (build-vector (vector-length vec)
                (λ ([i : (Refine [i : Natural]
                           (< i (vector-length vec)))])
                  (proc (safe-vector-ref vec i)))))
```

Now `vector-map`'s type indicates that the length of the returned
vector is equal to the length of the vector it received as input.

## `vector-copy`

`vector-copy` takes a vector `vec` and copies its elements in the
range [`start`,`end`) into a fresh vector:


```racket
> (vector-copy (vector 0 1 2 3) 1 3)
'#(1 2)
```

To precisely type this function, we will use a precondition to assert
the range specified by `start` and `end` is some sensible (although
possibly empty) portion of the input vector `vec`. A `Refine` allows
us to indicate that the length of the returned vector will always be
`(- end start)`:


```racket
(: vector-copy
   (All (A) (-> ([vec : (Vectorof A)]
                 [start : Natural]
                 [end : Natural])
                #:pre (vec start end)
                (<= start end (vector-length vec))
                (Refine [res : (Vectorof A)]
                  (= (- end start)
                     (vector-length res))))))
(define (vector-copy vec start end)
  (define len (- end start))
  (cond
    [(= 0 len) (vector)]
    [else
     (define res
       (make-vector len (safe-vector-ref vec start)))
     (let loop! ([i : Natural 0])
       (when (< i len)
         (define a (safe-vector-ref vec (+ start i)))
         (safe-vector-set! res i a)
         (loop! (+ 1 i))))
     res]))
```

## `vector-copy!`


`vector-copy!` is similar, but instead of returning a fresh vector,
the elements in the specified range are copied from a source vector
`s` into a destination vector `d`:


```racket
> (define s (vector "p" "e" "a" "r"))
> (define d (vector "" "" "" "" ""))
> (vector-copy! d 1 s 1 4)
> d
'#("" "e" "a" "r" "")
```

Here the precondition is larger because it includes constraints that
must hold with respect to both vectors `s` and `d`, i.e. the range in
the source `s` [`s-start`,`s-end`) must be a valid, and the
destination vector `d` must have a corresponding valid range
[`d-start`,`(+ d-start (- s-end s-start))`):


```racket
(: vector-copy!
   (All (A) (-> ([d : (Vectorof A)]
                 [d-start : Natural]
                 [s : (Vectorof A)]
                 [s-start : Natural]
                 [s-end : Natural])
                #:pre (d d-start s s-start s-end)
                (and (<= s-start s-end (vector-length s))
                     (<= d-start (vector-length d))
                     (<= (- s-end s-start)
                         (- (vector-length d) d-start)))
                Void)))
(define (vector-copy! d d-start s s-start s-end)
  (define count (- s-end s-start))
  (let loop! ([i : Natural 0])
    (when (< i count)
      (define a (safe-vector-ref s (+ s-start i)))
      (safe-vector-set! d (+ d-start i) a)
      (loop! (+ 1 i)))))
```

## quicksort

Finally, here is a version of quicksort that has been typed
using dependent function types to sort vectors of reals:

```racket
(: quicksort! (-> (Vectorof Real) Void))
(define (quicksort! A)
  (quicksort-helper! A 0 (- (vector-length A) 1)))

(: quicksort-helper! (-> ([A : (Vectorof Real)]
                   [lo : Natural]
                   [hi : Integer])
                  #:pre (A lo hi)
                  (and (<= lo (vector-length A))
                       (< hi (vector-length A)))
                  Void))
(define (quicksort-helper! A lo hi)
  (when (< lo hi)
    (define pivot (partition! A lo hi))
    (quicksort-helper! A lo (- pivot 1))
    (quicksort-helper! A (+ pivot 1) hi)))


(: swap! (-> ([A : (Vectorof Real)]
              [i : Natural]
              [j : Natural])
             #:pre (A i j)
             (and (< i (vector-length A))
                  (< j (vector-length A)))
             Void))
(define (swap! A i j)
  (define tmp (safe-vector-ref A i))
  (safe-vector-set! A i (safe-vector-ref A j))
  (safe-vector-set! A j tmp))



(: partition! (-> ([A : (Vectorof Real)]
                   [lo : Natural]
                   [hi : Natural])
                  #:pre (A lo hi)
                  (< lo hi (vector-length A))
                  (Refine [pivot : Natural]
                    (<= lo pivot hi))))
(define (partition! A lo hi)
  (define pivot (safe-vector-ref A lo))
  (let outer-loop!
    ([i : (Refine [n : Natural] (< lo n))
        (+ 1 lo)]
     [j : (Refine [n : Natural] (and (<= n hi)
                                     (<= lo n))) hi])
    (let i-loop!
      ([i : (Refine [n : Natural] (< lo n))
          i])
      (cond
        [(and (<= i j)
              (< (safe-vector-ref A i) pivot))
         (i-loop! (+ i 1))]
        [else
         (let j-loop!
           ([j : (Refine [n : Natural]
                   (and (<= n hi) (<= lo n))) j])
           (cond
             [(and (>= (safe-vector-ref A j) pivot)
                   (>= j i))
              (j-loop! (- j 1))]
             [(> i j) (swap! A lo j)
                      j]
             [else
              (swap! A i j)
              (outer-loop! i j)]))]))))
```

# Typed/Untyped interaction

Typed Racket will generate dependent function contracts (e.g. [->i](https://docs.racket-lang.org/reference/function-contracts.html?q=-%3Ei#%28form._%28%28lib._racket%2Fcontract%2Fbase..rkt%29._-~3ei%29%29)
contracts) to protect dependent functions when they are used in
untyped modules:

```racket
#lang racket/base

(require "safe-vector-ops.rkt")

(safe-vector-ref (vector 0 1) 2)

;; produces:
;;
;; safe-vector-ref: contract violation
;;  #:pre condition violation; ...
```

As always, care should be taken when designing APIs that involve
contract boundaries which may be frequently crossed by performance
sensitive code since each boundary crossing will involve runtime
checks for the respective types.

# Final Notes

## Linear integer arithmetic

Typed Racket today only reasons about linear integer constraints,
i.e. arithmetic constraints expressible with `<=`, `and`, and `or`
over [linear
combinations](https://en.wikipedia.org/wiki/Linear_combination) of
select program terms. If the specification you wish to encode in types
requires non-linear arithmetic, Typed Racket will not be able to
verify those constraints.

## Dependent function type limitations

Dependent function types currently only support mandatory positional
arguments. In the future we plan to add optional, rest, and keyword
argument support.

Currently functions with dependencies between arguments and/or
preconditions cannot be used within a `case->` form.

There is not yet support for dependent struct types.

## Type checking the above examples in Racket v6.11

<a name="makevectortype"></a>

To type check the examples in this post on Racket v6.11, the following
snippet must be added to the module:

```racket
(require typed/racket/unsafe)

(unsafe-require/typed/provide
 typed/racket/base
 [make-vector (All (A) (-> ([n : Natural]
                            [val : A])
                           (Refine [v : (Vectorof A)]
                             (= n (vector-length v)))))])
```

This axiomatizes a more specific type for `make-vector`. Any snapshot
builds of Racket from November 10 2017 or later will not require this
snippet.

## Bugs

If you have found a bug or have an idea for how these features might
be more useful, feel free to open an issue on Typed Racket's [github
repo](https://github.com/racket/typed-racket/issues).
