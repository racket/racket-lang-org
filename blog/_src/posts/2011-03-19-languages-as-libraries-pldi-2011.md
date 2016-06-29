
    Title:Languages as Libraries, PLDI 2011
    Date:2011-03-19T10:58:00.001-04:00
    Tags:

*posted by Sam Tobin-Hochstadt*

We've just finished up the [final version](http://bit.ly/langlib) of our [PLDI 2011](http://pldi11.cs.utah.edu/) paper on language extension in Racket.  The paper describes how the module system and the syntax system work together to support new languages with new static semantics, such as Typed Racket.  Here's the abstract:

> Programming language design benefits from constructs for extending the syntax and semantics of a host language.  While C's string-based macros empower programmers to introduce notational shorthands, the parser-level macros of Lisp encourage experimentation with domain-specific languages.  The Scheme programming language improves on Lisp with macros that respect lexical scope.

> The design of Racket—a descendant of Scheme—goes even further with the introduction of a full-fledged interface to the static semantics of the language.  A Racket extension programmer can thus add constructs that are indistinguishable from “native” notation, large and complex embedded domain-specific languages, and even optimizing transformations for the compiler backend.  This power to experiment with language design has been used to create a series of sub-languages for programming with first-class classes and modules, numerous languages for implementing the Racket system, and the creation of a complete and fully integrated typed sister language to Racket's untyped base language.

> This paper explains Racket's language extension API via an implementation of a small typed sister language.  The new language provides a rich type system that accommodates the idioms of untyped Racket. Furthermore, modules in this typed language can safely exchange values with untyped modules.  Last but not least, the implementation includes a type-based optimizer that achieves promising speedups.  Although these extensions are complex, their Racket implementation is just a library, like any other library, requiring no changes to the Racket implementation.

To learn how to implement your own new language in Racket, start with [this documentation](http://docs.racket-lang.org/guide/languages.html).

<!-- more -->



* * *

It would be a good idea if the programmer can write very documentation comments for variables and functions (as in java / ** * /). And in the IDE climbs tips with documentation to functions. And to add intelligent code insertion, and auto completion of the input string.

— *Алексей, 22 March 2011*

* * *

Probably the best way to add documentation strings to Racket would be to add a keyword option to the "define" form:

(define (foobar . args) #:doc "Blah blah blah" . body)

(define myvar myvalue #:doc "My value")

(define myvar #:doc "It's mine" myvalue)

— *Blog User, 6 April 2011*

* * *

Are the Typed Racket versions of the benchmarks mentioned in section 7, available anywhere ? 
Thanks

— *therac25, 24 June 2011*

* * *

