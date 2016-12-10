
    Title:Submodules
    Date:2012-06-03T02:10:00.000-04:00
    Tags:

*posted by Matthew Flatt*

A Racket submodule is a module that is syntactically nested within another module. Submodules will be supported in the next release of Racket, and they are available in the current [pre-release version](http://pre.racket-lang.org/installers/).

Submodules provide nested namespaces, but that kind of nesting is already available through forms like `define-package`. The power of submodules is that they can be separately loaded and separately run relative to their enclosing modules, in the same way that top-level modules can be separately load and run. This separation of dependencies means that submodules can be used to add code and information to modules—such as tests, documentation, and parsing information—that is loaded only when specifically requested.


The `main` Submodule
---

One use of a submodule is to declare a `main` submodule. A `main` submodule is instantiated when the enclosing module is run as the main program, but not when the enclosing module is used as a library.


`"fish.rkt"`

```racket
#lang racket/base
(provide fish)
(define fish '(1 2))
(module+ main
  (map displayln fish))
```

`"sum-fish.rkt"`

```racket
#lang racket/base
(require "fish.rkt")
(apply + fish)
```

The `"fish.rkt"` module exports fish as a list of numbers.  Running `"sum-fish.rkt"`, which imports `"fish.rkt"`, prints the sum of the numbers. Running `"fish.rkt"` directly, however, triggers the instantiation of the main submodule within `"fish.rkt"`, which displays each number in fish on its own line.

A `(module+ main ....)` declaration is similar to the Python `if __name__ == "__main__":` idiom, but with a significant difference. Importing `"fish.rkt"` into another module ignores the main submodule completely, so that the main submodule’s code and its dependencies aren’t loaded.


Unit Tests
---

Another use for submodules—and one where independent loading matters more than for `"fish.rkt"`’s `main`—is for test suites.  A `main` submodule could be used for tests, so that running the module runs its tests, but our preferred convention is to declare a `test` submodule:






`"fish2.rkt"`

```racket

#lang racket/base
(provide fish)
(define fish '(1 2))
(module+ test
  (require rackunit)
  (check andmap number? fish))
```


The new `raco test` shell command runs the `test` submodule of a given module, so that `raco test fish2.rkt` checks that all the values of the `fish` list are numbers. The `test` submodule imports `rackunit` for its check form, but that import does not create a dependency on `rackunit` (which is a substantial library) for modules that import `"fish2.rkt"`; the dependency is only for the test submodule.

The `module+` form creates a dependency of the submodule on the enclosing module, since it implicitly imports all bindings of its enclosing module. The implicit import explains why the test submodule in `"fish2.rkt"` can use fish directly (i.e., it’s not simply because the submodule is syntactically nested). The implicit import includes all bindings from the enclosing module, including bindings that are not exported via provide, which supports unit tests for unexported functions.

Finally, the `module+` form splices together multiple declarations of a particular submodule, which is useful for interleaving definitions and tests:



`"fish3.rkt"`

```racket

#lang racket/base
(provide fish feed)
(module+ test (require rackunit))
(define fish '(1 2))
(module+ test (check andmap number? fish))
(define (feed n) (+ n 1))
(module+ test (check-equal? 3 (feed 2)))

```


Since tests are isolated to a submodule, it might make sense to “strip” tests from a set of modules to prepare them for distribution to end-users. Although we haven’t created the `raco strip` command, yet, it’s a likely future addition. In that way, submodules act like sections in an object-linking file format such as ELF.


Core Submodule Forms
---

The `module+` form is actually just a macro that expands to a more primitive form for declaring submodules. The primitive submodule forms are `module` and `module*`, which reflect the two different directions that module dependencies can run: the `module*` form allows the submodule to import its enclosing module, while the `module` form allows the enclosing module to import the submodule.

As a minor feature, submodules can be declared with `module` and used by a `require`—essentially the same within a module as interactively:


```racket

#lang racket/base
(module zoo racket/base
  (provide tiger)
  (define tiger "Tony"))
(require 'zoo)
tiger

```


More significantly, `module` allows a submodule to be free of any dependency on its enclosing module, while the enclosing module similarly has no obligation to import the submodule.

The `module*` form similarly implies no a priori dependency of the submodule on its enclosing module, except that a `#f` for the submodule’s initial import means an import of all of the enclosing module’s bindings. The `module+` form expands (after collecting all pieces of a submodule’s body) to a `module*` form with a `#f` initial import.


In-Source Documentation
---

A more interesting example is the `scribble/srcdoc` library, which supports documentation within a library’s source in a JavaDoc-like way:






`"fish4.rkt"`

```racket

#lang racket
(require scribble/srcdoc
         (for-doc racket/base scribble/manual))
(provide
 (thing-doc
  fish (listof number?)
  ("Our fish, each represented as a number.")))
(define fish '(1 2))
(provide
 (proc-doc/names
  feed (number? . -> . number?) (n)
  ("Feed 1 pound of food to the fish " (racket n) ".")))
(define (feed n) (+ n 1))
```


The `scribble/srcdoc` library provides `thing-doc` and `proc-doc`, which can be used instead of a plain `provide` form to attach both a contract and documentation to the exported binding. The contract is used at run time to guard uses of the value. The contract is also included in the documentation with hyperlinks to bindings that are used in the contract, such as `number?`.

In addition to provide forms, the `scribble/srcdoc` library provides `for-doc` for use within `require`. A `for-doc` imports forms that are used in the implementation of the documentation, as opposed to the implementation of the library. In `"fish4.rkt"`, `scribble/manual` is imported for the racket form that is used in the documentation of feed.

These forms from `scribble/srcdoc` work together to construct a `srcdoc` submodule that contains documentation for the enclosing module without creating any documentation-related run-time overhead for the enclosing module.  The module’s documentation is loaded from bytecode only when specifically requested from the `srcdoc` submodule for inclusion in a documentation build via `include-extracted`:




`"fish4.scrbl"`

```racket

#lang scribble/manual
@(require scribble/extract)
@title{Fish}
@defmodule["fish.rkt"]
@include-extracted["fish4.rkt"]

```




Implementing Languages
---

Top-level modules in Racket intentionally inherit no bindings from the top-level environment, so that (1) a module’s meaning is fixed independent of its load order or other effects, and (2) the initial import of a module can act as a “language” with complete control over the module’s meaning. That is, `#lang` is in principle the only top-level form in Racket. With only modules at the top level, however, macros cannot abstract over sets of top-level modules.

Submodules provide more flexibility, in that a macro defined within a module can abstract over a set of submodules. As it happens, abstracting over a set of submodules is useful for defining a new language for use with `#lang`.

A language for use with `#lang` is implemented by several pieces that live at different times, including the language’s parser, the language’s run-time support library, and the language’s syntax-coloring plug-in for DrRacket. Formerly, a programmer who implements a language with those three pieces was forced to write three different modules (or else tangle the different pieces in a single module, which invariably pulls too many dependencies into any one of them). Those pieces now can be in submodules, which opens the possibility for new abstractions that conveniently generate the various pieces of a language.

For example, if you want to define an `ocean` language that is `racket/base` plus `fish`, it’s enough to install the following module as `"main.rkt"` in an `"ocean"` collection (e.g., in an `"ocean"` directory is that is registered as a collection with the command `raco link ocean`):

```racket

#lang racket/base
(provide (all-from-out racket/base)
         fish)
(define fish '(1 2 3))
(displayln "Fish are swimming")
(module reader syntax/module-reader
  #:language 'ocean)
```


When Racket sees a module that starts `#lang ocean`, it does not simply load the `"main.rkt"` module of the `"ocean"` collection. Instead, `#lang` looks for a reader submodule of the `"main.rkt"` module. The reader module above does not depend on its enclosing module, so that parsing a module in the ocean language does not trigger the “Fish are swimming” printout. Instead, the `#:language 'ocean` part of the reader submodule indicates that a module parsed from `#lang ocean` starts by importing the `ocean` module, so the bindings of `ocean` are available in the program, and “Fish are swimming” will print when the program is run.


Submodules are Like...
---

At some level, syntactic nesting of modules is an obvious feature to include in a module system.  Nevertheless, Racket’s submodules are not like nested modules in most languages—including Python, Chez, or ML—where nesting is for namespace management and nested modules are always instantiated along with the enclosing module. Racket submodules can be used in a similar way, but the fact that submodules are separately loadable makes them available to solve a larger class of problems.

If I had to pick just one analogy, I’d say that submodules are most like a generalization of annotations in the Java sense. Java annotations allow the decoration of code with metadata, and the annotations are preserved through run time, so that annotations can be inspected in source, in compiled code, or reflectively at run time. Java annotations are limited to data, so that any abstraction or programatic interpretation of the data depends on yet another external tool and language, or else the code part (such as test to run for a `@Test` annotation) is difficult to separate from the main program. C# attributes are slightly more general, in that the data can have associated methods, but attribute code is still mingled with run-time code. Submodules generalize annotations to make them “live,” so that the language of annotations can include expressions, functions, and even syntactic extensions, while allowing the annotation/submodule code to stay separate from the base code.

For more information on submodules, see the pre-release Guide section.

<!-- more -->



* * *

That was enormously helpful.  I'd followed the discussion of submodules on the mailing list, but  hadn't quite "gotten" them until now.  (I particularly like that you focused on the _problem_ that this feature solves).

I hope you put this essay, or something like it, into the Racket Guide.

— *offby1, 3 June 2012*

* * *

