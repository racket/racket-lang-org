
    Title:Serializable Closures in PLT Scheme
    Date:2009-06-23T12:02:00.003-04:00
    Tags:

*posted by Jay McCarthy*

PLT Scheme supports an extensible [serialization](http://docs.plt-scheme.org/reference/serialization.html) system for structures. A structure is serializable if it has a `prop:serializable` property. There are many [properties](http://docs.plt-scheme.org/search/index.html?q=prop%3A) in PLT Scheme for other extensions, such as [applicable structures](http://docs.plt-scheme.org/reference/procedures.html#(def._((lib._scheme/base..ss)._prop~3aprocedure))) and [custom equality predicates](http://docs.plt-scheme.org/reference/booleans.html#(def._((quote._~23~25kernel)._prop~3aequal+hash))).

The [PLT Web](http://docs.plt-scheme.org/web-server/index.html) application development framework uses these features to provide [serializable continuations](http://docs.plt-scheme.org/web-server/stateless.html#(part._.Serializable_.Continuations)) through a number of source transformations and a serializable closure structure.

_Warning: This remainder post refers to features only available in the latest SVN revision of PLT Scheme._

I've recently made these closures more accessible to non-Web programs through `web-server/lang/serial-lambda`. Here's a demo:

```racket
#lang scheme
(require web-server/lang/serial-lambda
         scheme/serialize)

(define f
  (let ([z 5])
    (serial-lambda
     (x y)
     (+ x y z))))

(define (test-it)
  (printf "~S~n" (f 1 2))
  (let ([fs (serialize f)])
    (printf "~S~n" fs)
    (let ([df (deserialize fs)])
      (printf "~S~n" df)
      (printf "~S~n" (df 1 2)))))

> (test-it)
8
((2) 1 ((#"/Users/jay/Dev/svn/plt/collects/web-server/exp/test-serial.ss" . "lifted.6")) 0 () () (0 5))
#(struct:7a410aca70b31e88b4c2f0fe77fa7ffe:0 #)
8
```

Now, let's see how it is implemented. [`web-server/lang/serial-lambda`](http://svn.plt-scheme.org/plt/trunk/collects/web-server/lang/serial-lambda.ss) is thin wrapper around [`web-server/lang/closure`](http://svn.plt-scheme.org/plt/trunk/collects/web-server/lang/closure.ss), which has two syntax transformer functions: `define-closure!` which defines the closure structure and `make-closure` which instantiates the closure. (The two tasks are separated to easily provide a user top-level definition syntax for named closures with different free identifires, rather than simply anonymous lambdas with fixed free identifiers.)

`make-closure` does the following:


1. Expands the procedure syntax using [`local-expand`](http://docs.plt-scheme.org/reference/stxtrans.html#(def._((quote._~23~25kernel)._local-expand))), so it can use [`free-vars`](http://docs.plt-scheme.org/syntax/syntax-helpers.html#(def._((lib._syntax/free-vars..ss)._free-vars))) to compute the free identifires.

2. Uses `define-closure!` to define the structure and get the name for the constructor.

3. Instantiates the closure with the current values of the free identifiers.


The more interesting work is done by `define-closure!`. At a high-level, it needs to do the following:


1. Create a deserialization function.

2. Create a serialization function that references the deserializer.

3. Define the closure structure type that references the serializer.

4. Provide the deserializer from the current module so that arbitrary code can deserialize instances of this closure type.


These tasks are complicated in a few ways:


* The deserializer needs the closure structure type definition to create instances and the serializer needs the closure structure type to access their fields.

* The serializer needs the syntactic identifier of the deserializer so that `scheme/serialize` can [`dynamic-require`](http://docs.plt-scheme.org/reference/Module_Names_and_Loading.html#(def._((quote._~23~25kernel)._dynamic-require))) it during deserialization.

* The deserializer must be defined at the top-level, so it may be provided.

* All this may occur in a syntactic expression context.


Thankfully, the PLT Scheme [macro system](http://docs.plt-scheme.org/reference/Macros.html) is powerful to support all this.


* [`syntax-local-lift-expression`](http://docs.plt-scheme.org/reference/stxtrans.html#(def._((quote._~23~25kernel)._syntax-local-lift-expression))) allows a syntax transformer to lift an expression to the top-level of a module and returns the identifier it is bound to.

* [`syntax-local-lift-values-expression`](http://docs.plt-scheme.org/search/index.html?q=syntax-local-lift-values-expression) (added in 4.2.0.3) provides the same for expressions that return multiple values, such as [`make-struct-type`](http://docs.plt-scheme.org/reference/creatingmorestructs.html#(def._((quote._~23~25kernel)._make-struct-type))), which is used to define structures.

* [`syntax-local-lift-provide`](http://docs.plt-scheme.org/search/index.html?q=syntax-local-lift-provide) (added in 4.2.0.4) allows a syntax transformer to lift a provide to the top-level.
 

The only complicated piece is allowing the deserializer and serializer to refer to the closure structure constructor and accessors. This is easily accomplished by first defining lifting boxes that will hold these values and initializing them when the structure type is defined. This is safe because all accesses to the boxes are under lambdas that are guaranteed not to be run before the structure type is defined.

**An aside on the closure representation.** The closure is represented as a structure with one field: the environment. The environment is represented as a thunk that returns _n_ values, one for each of the free identifiers. This ensures that references that were under lambdas in the original syntax, remain under lambdas in the closure construction, so the serializable closures work correctly inside `letrec`. This thunk is applied by the serializer and the free values are stored in a vector. The closure also uses the `prop:procedure` structure property to provide an application function that simply invokes the environment thunk and binds its names, then `apply`s the original procedure syntax to the arguments.

**An aside on the serializer.** The deserializer is bound to lifted identifier which is represented in PLT Scheme as an unreadable symbol. Version 4.2.0.5 added support for (de)serializing these.

<!-- more -->



* * *

Awesome.  Serialization is one of the things I'm looking for right now, so I'm glad my favorite distro is making it easier.

— *Zachary, 3 January 2010*

* * *

This is very, very, shiny. I might use this in my master's thesis.

— *Alex, 12 February 2010*

* * *

