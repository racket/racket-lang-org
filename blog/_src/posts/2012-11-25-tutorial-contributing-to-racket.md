
    Title:Tutorial: Contributing to Racket
    Date:2012-11-25T17:02:00.001-05:00
    Tags:

*posted by Joe Gibbs Politz*

_Originally posted on [jpolitz.github.com](http://jpolitz.github.com/blog/2012/11/21/racket-contributing-tutorial.html)._



I've been a longtime user and sometimes fanboy of [Racket](http://racket-lang.org/), but aside from a few bug reports, before this week I hadn't contributed anything back to the language.  This week, I started using a little helper macro, which wasn't in the core utils, to make some of my testing easier.  I mentioned it to the super-friendly Racket community, they told me they liked it, and my pull request was merged within about 12 hours.



I've been using Racket for a while, so I knew roughly where to lookto put my code, tests, and documentation.  A newer user might not know,so this post outlines, in some detail, the steps I went through to puttogether a tiny feature extension for Racket.  





A Tiny Feature
---

I'm dabbling in the implementation of a [small scripting language called Pyret](http://github.com/brownplt/pyret-lang) to study features of scripting objects.  The language has a parser, which generates AST nodes.  The nodes keep track of their location in the original program for error reporting, unbound identifier reporting, and the like.  I wanted to write some test cases for our

parser, which generates things like:


```racket
> (parse "o.x")
(s-block (srcloc "parse-tests.rkt" 1 0 #f #f)
         (list (s-dot
                (srcloc "parse-tests.rkt" 1 0 #f #f)
                (s-id (srcloc "parse-tests.rkt" 1 0 #f #f) 'o)
                'x)))
```


A ton of detail is in the output keeping track of line number information.  But I don't want to have to type out the line numbers and get them right for each test.  I'd like to write:


```racket
(check-match (parse "o.x")
(s-block _ (list (s-dot _ (s-id _ 'o) 'x))))
```

Which checks that all the things I care about for the parse are true: the program parses to a block of code, with a single statement, which is a dot expression of the identifier `o` and the symbol `x`.  With a little help from [Jonah Kagan](http://www.jonahkagan.me/), I produced a macro that does exactly that, and works nicely with [RackUnit](http://docs.racket-lang.org/rackunit/api.html), Racket's unit-testing framework ([see it in action](https://github.com/brownplt/pyret-lang/blob/master/src/tests/parse-tests.rkt#L36), with a slightly different name).



I thought `check-match` was pretty useful, and figured I'd see if the Racket folks at large would agree.  I [wrote a message](http://www.mail-archive.com/dev@racket-lang.org/msg07427.html) to the Racket mailing list, figuring someone might think it was neat.  There was some [immediate](http://www.mail-archive.com/dev@racket-lang.org/msg07429.html) [positive](http://www.mail-archive.com/dev@racket-lang.org/msg07430.html) [feedback](http://www.mail-archive.com/dev@racket-lang.org/msg07428.html), so I decided to go ahead and try to add it.





Getting and Extending Racket
--- 

[Racket's repo](http://github.com/plt/racket) is hosted on Github.  The easiest way to contribute is to [fork it](https://help.github.com/articles/fork-a-repo), and then check out your own copy.  The check-out and build process is fairly standard; you should, however, make a directory called `build/` to hold the binaries that will be created:


```bash
$ git clone git://github.com/<your-username>/racket.git
$ cd racket/src
$ mkdir build
$ cd build
$ ../configure
$ make
$ make install
```

This takes about 20-30 minutes, and installs all the necessary Racket binaries locally in place (no `sudo` or anything needed).



Next up was to find RackUnit and the code I'd need to extend.



Most of what goes on in Racket's core utilities happens in _collections_, found in the `collects/` directory of the base directory of the checkout.  For my implementation, I'd be looking at `collects/rackunit`.



I want to implement a new kind of `check`, so let's find that in RackUnit.  Here's what the RackUnit directory looks like:


```bash
$ ls collects/rackunit/
compiled           gui.rkt   main.rkt  scribblings  tool.rkt
docs-complete.rkt  info.rkt  private   text-ui.rkt
```

The `private/` directory contains most of the internals of the built-in collections' behavior, so let's look at that:


```bash
$ ls collects/rackunit/private/
base.rkt        counter.rkt     location.rkt        test-case.rkt
check-info.rkt  format.rkt      monad.rkt           test.rkt
check.rkt       gui             name-collector.rkt  test-suite.rkt
compiled        hash-monad.rkt  result.rkt          text-ui-util.rkt
```

Well, `check.rkt` seems awfully promising.  It defines all of the checks that you can see in the RackUnit docs:


```racket
(provide ...
         check-eq?
         check-eqv?
         check-equal?
         check-=
         ...)

(define-binary-check (check-eq? eq? expr1 expr2))

(define-binary-check (check-eqv? eqv? expr1 expr2))

(define-binary-check (check-equal? expr1 expr2)
  (equal? expr1 expr2))

(define-simple-check (check-= expr1 expr2 epsilon)
  ( (magnitude (- expr1 expr2)) epsilon))

...
```

But before I go sticking my code in there willy-nilly, it's important to realize there are three things that need to go with a commit like this:


* Tests

* Implementation

* Documentation

We'll build up our commit in those stages.

Adding Tests
---

First, I need to know how I'm going to test this to make sure I don't screw anything up with my edits.  There's actually a whole collection for tests in `collects/tests/`, which includes a rackunit subdirectory.  Conveniently, this has been further divided into files that correspond to the files from the RackUnit collection itself:

```bash
$ ls collects/tests/rackunit/
all-rackunit-tests.rkt  monad-test.rkt                
base-test.rkt           pr10950.rkt                   
check-info-test.rkt     result-test.rkt               
check-test.rkt          run-tests.rkt                 
counter-test.rkt        standalone-check-test.rkt     
format-test.rkt         standalone.rkt                
hash-monad-test.rkt     standalone-test-case-test.rkt
location-test.rkt       test-case-test.rkt
```

So, we can add a few expected uses to `check-test.rkt`, which will be tested against the implementation.  I found the end of the check-tests, and inserted some simple test cases there, using the existing style of the file:

```racket
...
   ;; existing tests
   (test-case "Use of check as expression"
              (for-each check-false '(#f #f #f)))
   (test-case "Use of local check as expression"
              (let ()
                (define-simple-check (check-symbol? x)
                  (symbol? x))
                (for-each check-symbol? '(a b c))))
   ;; my added tests
   (test-case "Trivial check-match test"
              (check-match "dirigible" _))

   (test-case "Simple check-match test"
              (check-match (list 1 2 3) (list _ _ 3)))

   (test-case "check-match with a nested struct"
              (let ()
                (struct data (f1 f2 f3))
                (check-match (data 1 2 (data 1 2 3))
                             (data _ 2 (data _ _ _)))))
```

Implementation and Running Tests
---

With the tests written, it's safe to go back and add my implementation to `check.rkt`, since I'll know if I've succeeded or not via these tests.  I added my implementation there, with some comment caveats about how `check-match` differs from other checks:

```racket
;; NOTE(jpolitz): This match form isn't eager like the others, hence
;; the define-syntax and the need to carry around location information
(define-syntax (check-match stx)
  (syntax-case stx ()
   ((_ actual expected pred)
     ;;... implementation here ...
   )))
```

The actual implementation of `check-match` is turns the pieces into an instance of `match` that yields true or false depending on if the value was matched.  Here's the essence:

```racket
(define-syntax check-match
  (syntax-rules ()
    [(_ actual expected pred)
     (let ([actual-val actual])
      (check-true (match actual-val
                   [expected pred]
                   [_ #f])))))]
    [(_ actual expected)
     (check-match actual expected #t)]))
```

In reality, this gives lousy error reporting, so the [actual implementation](https://github.com/plt/racket/blob/e264e4148884f0675bd21e889525ccb7239eb4b4/collects/rackunit/private/check.rkt#L286) leverages the helpful [with-check-info](http://docs.racket-lang.org/rackunit/api.html#(form._((lib._rackunit/main..rkt)._with-check-info))) form to populate the test with reporting information for failures.

With the implementation in place, it's time to run the tests, and figure out if what I did broke anything.  To run a particular test suite, Racket provides a tool called `raco` that was built by the `make install` above. To run our tests, we do (from the base `racket/` directory):

```bash
$ ./bin/raco test collects/tests/rackunit
```

I iterated through this a few times to suss out all the minor bugs in what I'd written.  I also wanted to check that my tests were actually adding to the count, so I compared to the version without my changes by doing:

```bash
$ git stash
# stores my edits temporarily in git's stash
$ ./bin/raco test collects/tests/rackunit
# Output including "120 tests passed, 0 tests failed"
$ git stash apply
# re-applies my edits
$ ./bin/raco test collects/tests/rackunit
# Output including "127 tests passed, 0 tests failed", which seems good,
# since I wrote 7 new tests
```

So, I'm happy with my implementation.  All that's left is to write something down about this feature that others will be able to find it and use it in the future.

Adding Documentation
---

Racket uses a tool called [Scribble](http://docs.racket-lang.org/scribble/index.html) for documentation, and by convention, a collection's documentation is stored in the `scribblings/` subdirectory of the collection:

```bash
$ ls collects/rackunit/scribblings/
acknowledgements.scrbl  control-flow.scrbl  philosophy.scrbl
api.scrbl               file.rkt            quick-start.scrbl
base.rkt                file-test.rkt       rackunit.scrbl
check.scrbl             internals.scrbl     release-notes.scrbl
compiled                misc.scrbl          ui.scrbl
compound-testing.scrbl  overview.scrbl      utils.scrbl
```

Keeping with the theme, we'll be editing `check.scrbl` which is the file that's used to generate [this section](http://docs.racket-lang.org/rackunit/api.html#(part._.Checks)) of the RackUnit documentation.

Reading over the existing docs, I notice that our new feature is violating one of the principles of the existing documentation:


> Although checks are implemented as macros, which is necessary to grab source location, they are conceptually functions.  This means, for instance, checks always evaluate their arguments.


Based on [Robby's recommendation](http://www.mail-archive.com/dev@racket-lang.org/msg07435.html) (the mailing list is helpful and responsive again!) I simply added a caveat "(with the exception of @racket[check-match] below)", and moved on to adding actual documentation for `check-match`.

Scribble does two very cool things when documenting definitions. First, it has explicit syntax for telling the documentation system that you're introducing a new identifier that should be indexed and linkable. Second, it lets you write Racket code examples directly into the documentation, and even runs them and renders their results inline into the documenation.  Here's a snippet of what I add:

```racket
@defform*[((check-match v pattern)
           (check-match v pattern pred))]{

A check that pattern matches on the test value.  Matches the test value
@racket[v] against @racket[pattern] as a @racket[match] clause.  If no
@racket[pred] is provided, then if the match succeeds, the entire check
succeeds.  For example, this use succeeds:

@interaction[#:eval rackunit-eval
  (check-match (list 1 2 3) (list _ _ 3))
]

This check fails to match:

@interaction[#:eval rackunit-eval
  (check-match (list 1 2 3) (list _ _ 4))
]
```

There are a few things going on here:


* `@defform` tells Scribble that this is a new syntactic form that should be indexed. Scribble figures out the the name is `check-match`, and adds links for it to the table of contents and enters it in the search index.



* `@racket[v]` tells Scribble to render `v` as Racket code, and Scribble is also smart enough to know that `v` is the same `v` in the definition, and creates a back link for it.



* `@interaction[#:eval rackunit-eval ... ]` blocks indicate expressions that should be run, with their output rendered after them.  This makes for beautiful docs with examples inline to show users exactly what their getting.
To build the docs, we run:

```bash
$ ./bin/raco setup collects/rackunit
```

Then, the docs will appear in the local documentation directory.  I can then open them up in a web browser and see the results (note the local url ending api.html; that's the local path to the documentation that's been installed):


Looks good!

Letting Racketeers Know
---

I packaged everything up in a [single commit](https://github.com/jpolitz/racket/commit/023d2278c1bb9819451790d774ae8e67a5d46f22), and sent the whole thing off to the Racket folks with a [pull request](https://github.com/plt/racket/pull/171).  They then reviewed it and incorporated it into [their HEAD](https://github.com/plt/racket/commit/e264e4148884f0675bd21e889525ccb7239eb4b4) the next day.


The Racket folks maintain a list of [Intro Projects](https://github.com/plt/racket/wiki/Intro-Projects), so there's easy places to start if you want to follow this tutorial and get involved!

<!-- more -->



* * *

For reference, cross-ref with Greg's post on infrequent contributions to Racket:
www.greghendershott.com/2013/04/a-guide-for-infrequent-contributors-to-racket.html

— *Laurent, 30 April 2013*

* * *

