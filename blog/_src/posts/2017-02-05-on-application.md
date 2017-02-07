    Title: On Application
    Date: 2017-02-05T15:17:41
    Tags:

*posted by Jack Firth*

Today I wanted to write about function application. Specifically, how to redefine and customize
application with Racket. We'll also look at some Racket packages the define interesting and useful
forms of function application.

## The theory

Application defines one half of *lambda calculus*, the formal model underlying much of modern
functional programming. The other half is *abstraction*, which creates new functions. Creating and
applying functions lies at the heart of Racket and many other functional languages.

So how are functions applied in Racket? What makes `(if (< 5 10) 'foo 'bar)` a macro use and
`(< 5 10)` a function use?

That's actually a trick question, because function application *is* a macro in Racket. During macro
expansion, the Racket expander inspects the first element of an expression to determine if it has a
binding to a macro. If it doesn't, rather than assume the expression is a function application, the
expander inserts an *artificial* identifier named `#%app`[racket] into the expression. So in the
above example, the expression `(< 5 10)` is converted to `(#%app < 5 10)`. This `#%app`[racket]
identifier doesn't refer to a single specific `#%app`[racket] like the `if`[racket] refers to
`if`[racket] from `racket/base`[racket], rather it refers to whatever the enclosing environment
defines `#%app`[racket] to be (which by default means ordinary function application from
`racket/base`[racket]).

However, imported modules can provide their own definitions of function application by providing an
`#%app`[racket] macro. Let's define our own `#%app`[racket] that, in addition to applying a
function, prints out a trace message. First let's define a helper function to implement the tracing:

<!-- more -->

```racket
#lang racket
(define (trace f . args)
  (printf "Applying ~v to arguments ~v\n" f args)
  (apply f args))
```

We use `~v`[racket] to more clearly see the difference between strings, symbols, and numbers. Now,
we can write a macro to insert a call to our function wherever we use the macro:

```racket
#lang racket
(define-syntax-rule (trace-app f arg ...)
  (trace f arg ...))
```

This macro is very simple, and on it's own isn't really useful at all (we could just call `trace`
directly). However, we can provide this macro with the name `#%app`[racket] to trigger the automatic
use of `trace-app` whenever a function call is written. We'll move our `trace` and `trace-app`
definitions into a submodule to see this in action without multiple files:

```racket
#lang racket
(module trace racket
  (define (trace f . args)
    (printf "Applying ~v to arguments ~v\n" f args)
    (apply f args))
  (define-syntax-rule (trace-app f arg ...)
    (trace f arg ...))
  (provide (rename-out [trace-app #%app])))
```

Now, with a simple `require`[racket] statement and these seven lines of code, we can trace the order
of evaluation of all expressions in a Racket module.

```racket
> (require 'trace)

> (+ (string-length "racket")
     (string-length "application"))
Applying #<procedure:string-length> to arguments '("racket")
Applying #<procedure:string-length> to arguments '("application")
Applying #<procedure:+> to arguments '(6 11)
17
```

## The practice

Redefinition of `#%app`[racket] is occasionally used when defining new languages, but a much more
pedestrian use is to add some notational shorthand to make certain constructs more convenient. For
example, consider the [`fancy-app`][1] package. This package provides an `#%app`[racket] macro that
behaves just like normal function application *unless* one or more underscores are used. In that
case, the function application is converted to a lambda that takes as input one argument for each
underscore. For example, `(format "Hello ~a" _)` is equivalent to
`(lambda (v) (format "Hello ~a" v))`. This is especially useful for whipping up quick lambdas as
arguments to functions like `map` and `filter`:

```racket
> (require fancy-app)
> (map (format "Hello ~a, how are you today?" _)
       (list "Alice" "Bob" "Eve"))
'("Hello Alice, how are you today?"
  "Hello Bob, how are you today?"
  "Hello Eve, how are you today?")
```

The `rackjure`[racket] package redefines `#%app` to make working with nested dictionaries easier.
Dictionaries can be used to get and set values for keys when used as procedures, and when
dictionaries are the *second* value of a function application the first value is interpreted as a
key and the dictionary's associated value is looked up. See [the documentation][2] for details.

[1]: https://github.com/samth/fancy-app
[2]: http://docs.racket-lang.org/rackjure/index.html#%28part._dict-app%29
