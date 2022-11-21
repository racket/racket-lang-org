#lang scribble/manual

Title: Shallow and Optional Typed Racket
Date: 2022-11-20T23:43:38
Tags: Typed Racket

@(require
   scribble/example
   (only-in scribble/eval interaction-eval interaction/no-prompt)
   (for-label typed/racket/base))

@(define deep-eval (make-base-eval #:lang 'typed/racket))
@(define shallow-eval (make-base-eval #:lang 'typed/racket/shallow))
@(define optional-eval (make-base-eval #:lang 'typed/racket/optional))

@(define (tech/reference . text)
   (apply tech text #:doc '(lib "scribblings/reference/reference.scrbl")))

@(define-syntax-rule (module-example #:eval ev #:label lbl lang-datum mod-code ... ex-code)
   (list
     (if lbl (list lbl) '())
     (racketmod lang-datum mod-code ...)
     (interaction-eval #:eval ev mod-code ...)
     (interaction/no-prompt #:eval ev ex-code)))

With the @hyperlink["https://blog.racket-lang.org/2022/11/racket-v8-7.html"]{Racket 8.7}
release, Typed Racket includes two languages that
weaken the run-time behavior of types: Shallow Typed Racket and Optional Typed Racket.
Whereas normal Typed Racket types (@italic{Deep} types) enforce guarantees that any module
can depend on, Shallow types enforce only local invariants in typed code, and
Optional types enforce nothing.
In return, Shallow and Optional types add less overhead to gradual interactions.
Code often runs faster and simpler than with Deep types.

Shallow Typed Racket and Optional Typed Racket use the same static types and typechecker as
normal (Deep) Typed Racket.


@section{Background: Typed--Untyped Interaction}

A key feature of Typed Racket is that it allows typed code to interact with
untyped code.
An untyped module can import from a typed one with a normal @racket[require] form,
and a typed module can import from an untyped one by using a @racket[require/typed]
form:

For example, if an untyped module provides a struct and a function:

@racketmod[
racket
(code:comment "distance.rkt")

(provide (struct-out pt)
         distance)

(struct pt (x y))

(code:contract distance : pt pt -> real)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-y p1))))))
]


then a typed module can import the untyped bindings:

@racketmod[
typed/racket

(require/typed "distance.rkt"
  [#:struct pt ([x : Real] [y : Real])]
  [distance (-> pt pt Real)])

(distance (pt 3 5) (pt 7 0))
]

So far so good.
One program combines untyped and typed code.

Now, what if the declared types are wrong?

The module below gives a wrong type to the distance function.
This type expects an integer result instead of a real number:

@racketmod[
typed/racket

(require/typed "distance.rkt"
  [#:struct pt ([x : Real] [y : Real])]
  [distance (-> pt pt Integer)])

(distance (pt 3 5) (pt 7 0))
]

Even with the wrong type, the program still typechecks (!)
because Typed Racket does not analyze untyped code.
It assumes the @racket[require/typed] types are correct and moves on.

But this program does have a type error.
At run-time, the call to distance returns a float instead of an integer,
contradicting the static type.

If we want to catch the error, then Typed Racket needs to enforce types at run-time
when typed and untyped code interact.


@section{Enforcing Type Boundaries}

By default, Typed Racket compiles types to higher-order contracts.
The function type @racket[(-> pt pt Integer)], for example, compiles to a function
contract that will raise an exception if gets attached to a function that
eventually returns a non-integer result.

Contracts enforce types with strong guarantees and offer useful debugging
information if an error occurs.
But they can also be expensive, especially when large, mutable, or higher-order
values frequently cross boundaries.
These high costs inspired a long search for cheaper ways to enforce types
than the standard @italic{Deep} strategy.

Two promising alternatives are @italic{Shallow} and @italic{Optional} types,
neither of which use higher-order contracts.

Shallow types use lightweight assertions called @italic{shape checks} to provide
a basic soundness guarantee.
Instead of putting heavy contracts at module boundaries, Shallow Typed Racket rewrites
typed code to incrementally check the shape of values.

Optional types use no checks.
They are completely unreliable for reasoning about typed-untyped interactions.
But, they also have zero cost.


@section{How to Select an Enforcement Strategy}

The @hash-lang[] of a Typed Racket module decides how its types
behave at run-time.
To change strategies, change the language.

@itemlist[
  @item{
    Deep types:
    @racketmodname[typed/racket], @racketmodname[typed/racket/base],
    @racketmodname[typed/racket/deep], or @racketmodname[typed/racket/base/deep].
  }
  @item{
    Shallow types:
    @racketmodname[typed/racket/shallow] or @racketmodname[typed/racket/base/shallow].
  }
  @item{
    Optional types:
    @racketmodname[typed/racket/optional] or @racketmodname[typed/racket/base/optional].
  }
]

For a complete list of forms that change depending on the @hash-lang[], see
@secref["Forms_that_Depend_on_the_Behavior_of_Types"
         #:doc '(lib "typed-racket/scribblings/ts-reference.scrbl")]
in the Typed Racket Reference.


@section{Example: Fewer Run-time Checks}

Deep types can be significantly more expensive than Shallow and Optional.
For example, the two functions below expect a data structure and access part of the data:
@racket[list-first] returns the first element of a list
and @racket[vec-length] counts the number of elements in a vector.

@racketblock[
(: list-first (-> (Listof Real) Real))
(define (list-first l)
  (car l))

(: vec-length (-> (Vectorof Real) Index))
(define (vec-length v)
  (vector-length v))
]

When these functions get called from untyped code, they have very different
costs depending on the behavior of types:

@itemlist[
  @item{
    Deep types check all incoming data structures exhaustively.
    Lists undergo a full traversal that validates every list element, including unused ones.
    Vectors get wrapped in a @tech/reference{chaperone} to guard against future writes.
  }
  @item{
    Shallow types check the shape of the incoming data structures using
    @racket[list?] and @racket[vector?].
    Elements of these structures get checked only when they are used by typed code.
  }
  @item{
    Optional types check nothing at all.
  }
]

@secref{sec:further-reading} has links to larger examples where the costs of Deep types
are huge compared to Shallow and Optional.


@section{Example: Weaker Types, Simpler Behavior}

Shallow and Optional types raise fewer run-time errors than Deep.
In many cases, the lack of an error means that a bug goes undetected.
Deep finds the bug and the other two miss it because they skipped a run-time check.

But for some programs, the Deep types are too cautious.
They reject a program that could run safely.

One restrictive type in the Deep world is @racket[Procedure], the type that
describes any function.
Because this type says nothing about argument and return types,
Deep Typed Racket never allows calls to a procedure, even after a cast:

@module-example[#:eval deep-eval #:label #f
typed/racket (code:comment "or #lang typed/racket/deep")

(: call-proc (-> Procedure Symbol))
(define (call-proc f)
  ((cast f (-> Symbol Symbol)) 'hello))

(call-proc identity)
]


Shallow types do allow calls to a @racket[Procedure], after a cast:

@module-example[#:eval shallow-eval #:label #f
typed/racket/shallow

(: call-proc (-> Procedure Symbol))
(define (call-proc f)
  ((cast f (-> Symbol Symbol)) 'hello))

(call-proc identity)
]


Optional types also allow calls to a @racket[Procedure]:

@module-example[#:eval shallow-eval #:label #f
typed/racket/optional

(: call-proc (-> Procedure Symbol))
(define (call-proc f)
  ((cast f (-> Symbol Symbol)) 'hello))

(call-proc identity)
]


@section{Four-Way Interactions}

Typed--untyped interactions are much more exciting now that ``typed code'' can
be Deep, Shallow, or Optional.
These three styles of typed code can all interact with untyped code (of course),
and they can also interact with one another.

Types in this four-way world need to be enforced at run-time depending on how
they were defined:

@itemlist[
  @item{
    Deep types get enforced with contracts at all boundaries to non-Deep code.
    This means that Deep--Shallow and Deep--Optional interactions can be expensive.
  }
  @item{
    Shallow types guard the boundaries to Optional and untyped code with shape checks.
  }
  @item{
    Optional types never enforce themselves.
    But a Deep-typed or Shallow-typed client of Optional code will insert
    run-time checks as a consequence of their strategies.
  }
]

These checks between Deep, Shallow, and Optional may come as a surprise,
especially because all typed code gets validated by the same static type
checker.
But the checks are necessary because run-time interactions can mix untyped
values into some of these typed modules.
If an Optional module were to import an untyped function and send it to Deep-typed code
without a contract wrapper, it could break the Deep type guarantees.

@section{Reflections on Deep, Shallow, and Optional}

Deep types, Shallow types, and Optional types have complementary strengths.
When and where does each one work best?
Here are a few suggestions, based on
@secref["When_to_Use_Deep__Shallow__or_Optional_"
        #:doc '(lib "typed-racket/scribblings/ts-guide.scrbl")]
from the Typed Racket Guide:

@itemlist[
  @item{
    Deep types make the most of static checking and optimizations.
    Use them for self-sufficient groups of typed modules.
    Avoid them at high-traffic boundaries to untyped or to non-Deep code.
  }
  @item{
    Shallow types provide a weak but useful soundness guarantee at low cost.
    Use them in typed modules that frequently communicate with the untyped world.
    Avoid them, however, in large typed modules because every expression in typed
    code potentially needs a Shallow shape check.
  }
  @item{
    Optional types use types for static analysis and nothing more.
    Use them when you do not want types enforced at run-time.
  }
]

We are very excited to have these languages in the Typed Racket family.
Learning more about where they fit in practical applications and about how
developers tend to use them will be part of the adventure.


@section[#:tag "sec:further-reading"]{Further Reading}

@itemlist[
  @item{
     @secref["typed-untyped-interaction"
         #:doc '(lib "typed-racket/scribblings/ts-guide.scrbl")]
     in the Typed Racket Guide
  }
  @item{
    @secref["behavior-of-types"
         #:doc '(lib "typed-racket/scribblings/ts-reference.scrbl")]
    in the Typed Racket Reference
  }
  @item{
    For the theoretical and practical motivation:
    @linebreak[]
    Ben Greenman.
    @italic{Deep and Shallow Types for Gradual Languages}.
    PLDI 2022.
    @linebreak[]
    Paper at @url{https://www2.ccs.neu.edu/racket/pubs/g-pldi-2022.pdf}
    @linebreak[]
    Slides at @url{https://cs.brown.edu/people/bgreenma/publications/apples-to-apples/g-pldi-2022-slides.pdf}
  }
  @item{
    Shallow Typed Racket is based on the Transient semantics from Reticulated Python:
    @linebreak[]
    Michael M. Vitousek, Cameron Swords, and Jeremy G. Siek.
    @italic{Big Types in Little Runtime: Open-World Soundness and Collaborative Blame for Gradual Type Systems}.
    POPL 2017.
    @linebreak[]
    Paper at @url{https://dl.acm.org/doi/10.1145/3009837.3009849}
  }
  @item{
    Vitousek's dissertation has much more to say about Reticulated and
    Transient:
    @linebreak[]
    Michael M. Vitousek.
    @italic{Gradual Typing for Python, Unguarded}.
    PhD thesis, Indiana University, 2019.
    @url{https://hdl.handle.net/2022/23172}
  }
  @item{
    Greenman's dissertation says more about combining Deep and Shallow in
    Typed Racket:
    @linebreak[]
    Ben Greenman.
    @italic{Deep and Shallow Types}.
    PhD thesis, Northeastern University, 2020.
    @url{http://hdl.handle.net/2047/D20398329}
  }
]

@close-eval[deep-eval]
@close-eval[shallow-eval]
@close-eval[optional-eval]

