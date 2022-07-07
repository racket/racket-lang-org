#lang scribble/manual

Title: Shallow Typed Racket and Optional Typed Racket
Date: 2022-06-21T23:43:38
Tags: DRAFT, Typed Racket

@; helper = _src/posts/2021-01-24-racket-status.scrbl

@; TODO
@; - how to render at all
@; - how to render blog-style

@(require
   (for-label typed/racket/base))

As of the the Racket 8.7 release, Typed Racket (TR) includes two additional
languages: Shallow TR and Optional TR.
These languages use the same type checker as normal Typed Racket,
but weaken the run-time behavior of types to lower the performance cost
of interactions with untyped code.
Whereas normal TR types (@emph{Deep} types) enforce guarantees that any module
can depend on,
Shallow types enforce only local invariants in typed code,
and Optional types enforce nothing.


@; TODO quick intro for D S U + how to use?


@section{Background: Typed Untyped Interaction}
@; Background. Adapted from TS Guide.

One of the key features of Typed Racket is that it allows the combination
of both typed and untyped code in a single program.
An untyped module can import from a typed one with a normal @racket[require] form,
and a typed module can import from an untyped one by using a @racket[require/typed]
annotation to specify types for the untyped code.

For example, the following untyped module exports a struct @racket[_pt]
and a distance function to compare two points:

@racketmod[#:file "distance.rkt"
racket

(provide (struct-out pt)
         distance)

(struct pt (x y))

(code:contract distance : pt pt -> real)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-y p1))))))
]

The following typed module uses @racket[require/typed] to import
the struct and function, and then uses them both:

@racketmod[#:file "client.rkt"
typed/racket

(require/typed "distance.rkt"
               [#:struct pt ([x : Real] [y : Real])]
               [distance (-> pt pt Real)])

(distance (pt 3 5) (pt 7 0))
]

So far so good, but what if the declared types were wrong?

For example, typed code might (mistakenly) expect the distance function
to return integers instead of real numbers:

@racketmod[#:file "int-client.rkt"
typed/racket

(require/typed "distance.rkt"
               [#:struct pt ([x : Real] [y : Real])]
               [distance (-> pt pt Integer)])

(distance (pt 3 5) (pt 7 0))
]

The static type checker does not find a problem with this module.
It assumes the @racket[require/typed] declarations are correct and typechecks
the rest accordingly.
But if we were to run this program, the call to @racket[distance] would
return a float instead of an integer --- contradicting the static type.
That would be unsound!

The issue is that static types are making a claim about the behavior of untyped
code.
In particular, the types in the @racket[require/typed] boundary make a claim
about an untyped module.
Typed Racket does not check this claim statically.
So if we care about the integrity of types, Typed Racket needs to enforce them
dynamically whenever typed and untyped code interact.


@section{Enforcing Type Boundaries}

@; These contracts can, however, have a non-trivial performance impact.
@; For programs in which these costs are problematic, Typed Racket provides
@; two alternatives. All together, the three options are Deep, Shallow, and Optional types.

By default, Typed Racket uses contracts wherever typed and untyped code
interact to ensure strong types.
For the example above, the function type @racket[(-> pt pt Integer)] compiles
to a function contract that checks every result computed by the function.
When the call @racket[(distance (pt 3 5) (pt 7 0))] returns a float, the contract
stops the program at the boundary, before typed code gets access to a value that
contradicts its assumptions.

Contracts turn types into reliable predictions about the behavior of programs
that mix typed and untyped code.
They are also helpful for debugging.
If a type-based contract detects an error, it can point to a source code boundary
that needs to change: either the type at the boundary is inaccurate, or the untyped
component that it describes has a bug.

However, contracts can be expensive.
When large values frequently cross boundaries, contracts perform lots of run-time checks.
For higher-order values (functions, mutable data structures), there are additional costs
to allocate a higher-order contract and redirect future operations through it.
These costs are too high in some programs [CITE], which motivates a search for
other enforcement strategies.

@; @bold{Important caveat}: contracts such as the @racket[Integer] check from
@; above are performant. However, contracts in general can
@; have a non-trivial performance impact, especially with the use of first-class
@; functions or other higher-order data such as vectors.
@; 
@; Note that no contract overhead is ever incurred for uses of typed
@; values from another Deep-typed module.

Two promising alternatives are what we call Shallow and Optional types.
Both of these forgo contracts entirely; they allow gradual typing without
a huge overhead from contracts.
Consequently, they are much weaker than normal Typed Racket types.

All together the three alternatives are:

@itemlist[#:style 'ordered
  @item{
    @emph{Deep} types (normal @racket[#lang typed/racket]) get rigorously enforced with contract checks.
  }
  @item{
    @emph{Shallow} types get checked in typed code with lightweight assertions,
    or shape checks.
  }
  @item{
    @emph{Optional} types do not get enforced in any way.
  }
]

In terms of formal properties,
Deep types offer sound interactions with untyped code that any client can depend on (complete monitoring).
Shallow types are sound 


@;  ... CM, sound, NB-sound?? (maybe drop column 3, unclear what the title is)
@; Deep     | X | X | X
@; Shallow  |   | X | X
@; Optional |   |   | X


@; ... details?

@; Shallow checks work together within one typed module to enforce the assumptions that
@; it makes about untyped code.
@; 
@; In general, a shape check ensures that a value matches the top-level constructor
@; of a type.
@; Shape checks are always yes-or-no predicates (unlike contracts, which may wrap a
@; value) and typically run in constant time.
@; Because they ensure the validity of type constructors, shape checks allow Typed
@; Racket to safely optimize some programs---though not to the same extent as Deep
@; types.
@; 
@; @bold{Important caveats}: (1) The number of shape checks in a module grows in
@; proportion to its size. For example, every function call in Shallow-typed code
@; gets checked---unless Typed Racket is certain that it can trust the function.
@; Shallow types are therefore a poor choice for large, computationally-heavy
@; modules.
@; (2) Shallow types are only enforced in their immediate, local context.
@; For example, if typed code were to cast @racket[increment] to expect a string,
@; then the function could be called without an error.


@; @subsection{Optional Types: It's Just Racket}
@; 
@; Optional types do not ensure safe typed-untyped interactions.
@; In fact, they do nothing to check types at run-time.
@; A call to the increment function does not raise an error:
@; 
@; @examples[#:label #f #:eval the-eval (require 'client)]
@; 
@; Optional types cannot detect incorrect type assumptions
@; and therefore enable zero type-driven optimizations.
@; But, they also add no costs to slow a program down.
@; In general, the behavior of an Optionally-typed program is the same as that of
@; a Racket program that completely ignores type annotations.



@section{How to Use}

The @hash-lang[] of a Typed Racket module decides how its types
behave at run-time.
The standard languages @racketmodname[typed/racket] and @racketmodname[typed/racket/base]
use Deep types.
The other options are:

@itemlist[
  @item{
    @racketmodname[typed/racket/deep] and @racketmodname[typed/racket/base/deep] for Deep types
  }
  @item{
    @racketmodname[typed/racket/shallow] and @racketmodname[typed/racket/base/shallow] for Shallow types
  }
  @item{
    @racketmodname[typed/racket/optional] and @racketmodname[typed/racket/base/optional] for Optional types
  }
]

@; Switching languages affects only the run-time behavior of types.
@; Works "flawlessly" for modules that don't use the boundary APIs.
@; May have trouble for define-typed/untyped-id and require/untyped-contract.

For a list of forms that change when the @hash-lang[] changes:
@secref["Forms_that_Depend_on_the_Behavior_of_Types"
         #:doc '(lib "typed-racket/scribblings/ts-reference.scrbl")]


@section{Why does it matter. Practical examples.}

@; ... these are all expressiveness. Why not performance too? benchmarks are good. Abstract yes, but good.

@; TODO fill in examples!


Example 1: list check
- reason 1 = performance
- dissertation for details, order of mag speedup
- small example = list check

Example 2: procedure cast
- reason 2 = expressiveness
- procedure cast is obvious, can't forget


@section{General Comments on Deep, Shallow, and Optional}

Deep types, Shallow types, and Optional types have complementary strengths.
This raises an obvious question: when and where does each one work best?
Here are a few suggestions
from the Typed Racket Guide
@secref["When_to_Use_Deep__Shallow__or_Optional_"
        #:doc '(lib "typed-racket/scribblings/ts-guide.scrbl")]
:

@itemlist[
  @item{
    Deep types maximize the benefits of static checking
    and type-driven optimizations.
    Use them for tightly-connected groups of typed models.
    Avoid them when untyped, higher-order values frequently
    cross boundaries into typed code. Expensive boundary types
    include @racket[Vectorof], @racket[->], and @racket[Object].
  }
  @item{
    Shallow types are best for small typed modules that frequently
    interact with untyped code.
    This is because Shallow shape checks run quickly: constant-time for
    most types, and linear time (in the size of the type, not the value)
    for a few exceptions such as @racket[U] and @racket[case->].
    Avoid Shallow types in large typed modules that frequently call functions
    or access data structures because these operations may incur shape checks
    and their net cost may be significant.
  }
  @item{
    Optional types enable the typechecker and nothing else. Use them when
    you do not want types enforced at run-time.
  }
]


@section{Further Reading}

@itemlist[
  @item{
     @secref["typed-untyped-interaction"
         #:doc '(lib "typed-racket/scribblings/ts-guide.scrbl")]
  }
  @item{
    @secref["behavior-of-types"
         #:doc '(lib "typed-racket/scribblings/ts-reference.scrbl")]
  }
  @item{
    Michael M. Vitousek.
    @emph{Gradual Typing for Python, Unguarded}.
    PhD thesis, Indiana University, 2019.
    @url{https://hdl.handle.net/2022/23172}
  }
  @item{
    Ben Greenman.
    @emph{Deep and Shallow Types}.
    PhD thesis, Northeastern University, 2020.
    @url{http://hdl.handle.net/2047/D20398329}
  }
]


