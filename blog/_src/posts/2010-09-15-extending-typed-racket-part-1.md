
    Title:Extending Typed Racket, Part 1
    Date:2010-09-15T15:34:00.001-04:00
    Tags:

*posted by Sam Tobin-Hochstadt*

The Typed Racket team is pleased to announce a number of new additions
to our system.   We'll be writing a few blog posts about them, all of
which you can read here.


This post begins with the core of the Typed Racket type system.
The fundamental idea at the heart of Typed Racket is called occurrence
typing.  This is the technique that allows us to typecheck
existing Racket programs without requiring rewrites.  Here's a simple
example:

```racket
(if (number? x) (add1 x) 0)
```

The typechecker can figure out from the use of `number?`
that the second occurrence of `x` is always going to be a
number.  This simple form of occurrence typing is enough to take Typed
Racket a long way.  But because we want to be able to handle all the
sophisticated reasoning that programmers are already using to write
their Racket programs, we have been working on extending the system
further.
 
The new design of our system is described in a paper, [Logical Types for Untyped Languages](http://www.ccs.neu.edu/scheme/pubs/#icfp10-thf), in
the upcoming International Conference on Functional Programming.  The
introduction provides an overview that's acessible to any Racket
programmer, but here's the key example:

```racket
(cond
  [(and (number? x) (string? y)) — 1 —]
  [(number? x)                   — 2 —]
  [else                          — 3 —])
```

In expression 1, we know that `x` is a number and
`y` is a string.  In 2, we know that `x` is a
number and `y` is not a string, by the logical
properties of `and` and `cond`.  This form of
logical reasoning is enabled by the new foundation of the system, and
makes the entire system significantly more expressive.  


All of these improvements are available in the current version of
Racket.

<!-- more -->



* * *

> The typechecker can figure out from the use of number? that the occurrence of x is always going to be a number.

Presumably that should be "the second occurrence of x"?

— *alexey-rom, 15 September 2010*

* * *

