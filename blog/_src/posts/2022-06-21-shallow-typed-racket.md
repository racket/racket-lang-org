#lang scribble/manual

Title: shallow-typed-racket
Date: 2022-06-21T23:43:38
Tags: DRAFT

@; helper = _src/posts/2021-01-24-racket-status.scrbl

@; ;; TODO
@(require
   (for-label typed/racket/base))


_Replace this with your post text. Add one or more comma-separated
Tags above. The special tag `DRAFT` will prevent the post from being
published._

TR 8.7
Shallow Typed Racket
Optional Typed Racket
two new languages, same static semantics, change dynamic semantics
in particular weaken the run time checks
may improve the performance of apps


stay excited, remember the mentors like the plid talk


@; key elements:
@; 
@; - semantics of types = runtime errors
@;   = bad performance in some cases
@; 
@; - two new semantics for TR
@; 
@; - easy to switch
@; 
@; - recommendations
@; 
@; - more in docs & papers


@section{Typed Untyped Interaction and Type Soundness}
@; Background. From TS Guide.


@; @subsection{Why Shallow? Why Optional?}



@section{How to Use}



@section{Why does it matter. Practical examples.}

Example 1: procedure cast

Example 2: list check

Example 3: index of



@section{Recommendations. When to use?}

The TS Reference has more to say.
Future let's go exploring.
... need for S O unpredictable, quite a surprise, so who knows keep programming
and keep thinking

General tips (from the guide):
@secref["When_to_Use_Deep__Shallow__or_Optional_"
        #:doc '(lib "typed-racket/scribblings/ts-guide.scrbl")]

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


