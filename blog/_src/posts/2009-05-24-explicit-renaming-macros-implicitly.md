
    Title:Explicit Renaming Macros; Implicitly
    Date:2009-05-24T03:17:00.004-04:00
    Tags:

*posted by Eli Barzilay*

It's been one too many times that I hear respectable Schemers talk about how they like explicit renaming macros — not because they're more powerful, but because using them is close to using simple `defmacro`s.  In this post I'll show how you can easily write ER-like macros in PLT, just so I won't need to explain the same thing once again.

Disclaimers:

* If you're not interested in ER-macros, then you shouldn't read this.

* I'm not claiming that ER macros are not respectable, I'm just surprised at the knee jerk reaction to `syntax-case`.

* This is not an attempt at providing some portable library or even a PLT library. The intention is to show that `syntax-case`-style macros are "as convenient" as ER macros, if you really want to get down to that level.

* This is also not an attempt at any kind of formal claim of equivalence in any direction, only a demonstration that you can get the same kind of convenience.

* The bottom line here should be just the convenience point, addressed at people who like ER macros for that, and who think that `syntax-case` macros are somehow magical or that you lose the ability to play with S-expressions.

The important fact here is that while PLT's `syntax-case` macro system does not give you raw S-expressions, what you get is a simple wrapper holding them.  A macro is a syntax transformer: a function that consumes a syntax value and returns one.  For example:  

```racket
(define-syntax (foo stx)
    #'123)
```

is a macro that always expands to 123 (with `#'123` being the usual shorthand for `(syntax 123)`).

A syntax object in PLT Scheme (the input to macro functions) is an S-expression, with some lexical information added — this includes the lexical context (in an opaque form), source location, and a few more things.  To be more precise, a syntax value is a nested structure of wrappers holding lists and pairs, holding more wrappers, with identifiers at the leaves, where an identifier is a wrapper holding a symbol.  It's easy to strip off all wrappers using `syntax->datum` if you like to work with S-expressions, but you _don't_ want to strip it off of identifiers, since that will lose the important gravy.  (In fact, the `defmacro` library works by stripping off all information, even from identifiers, then reconstructing it by trying to match names in the output form with the original input.)

Instead of throwing away all information, what we want to do is preserve identifiers.  We can use `syntax->list` for this: this is a function that takes a syntax value that contains a list, and strips off the top-level extra information leaving you with a list of syntaxes for the sub-expressions (returning `#f` if the input syntax does not hold a list).  Once we have such a list, we can do the usual kind of processing with it, and when we're done turn the result back into a syntax using `datum->syntax` (which "borrows" the original input expression's information).  For example,  

```racket
(define-syntax (add1 stx)
    (let ([+ #'+])
      (datum->syntax stx `(,+ 1 ,@(cdr (syntax->list stx))) stx)))
```

That's a very simple example though; if you try something a little more complicated, you quickly find out that all this unwrapping is inconvenient:  

```racket
(define-syntax (mylet stx)
    (let ([*lambda #'lambda])
      (datum->syntax
       stx
       `((,*lambda ,(map (lambda (x) (car (syntax->list x)))
                         (syntax->list (cadr (syntax->list stx))))
                   ,@(cddr (syntax->list stx)))
         ,@(map (lambda (x) (cadr (syntax->list x)))
                (syntax->list (cadr (syntax->list stx)))))
       stx)))
```

(Note also the `*lambda` that is used to avoid the `lambda` expressions used in the meta-code.)

What can help here is some helper function that receive a syntax value as its input, and turn all wrapped lists into real lists recursively, but will leave identifiers intact:  

```(begin-for-syntax
    (define (strip stx)
      (let ([maybe-list (syntax->list stx)])
        ;; syntax->list returns #f if the syntax is not a list
        (if maybe-list (map strip maybe-list) stx))))
```

But as long as we're writing a syntax utility, we can make it do a litte more work: feed the resulting tree to _your_ transformer, grab its result, and do the necessary `datum->syntax` voodoo on it:  

```racket
(begin-for-syntax
    (define (er-like-transformer transformer)
      (define (strip stx)
        (let ([maybe-list (syntax->list stx)])
          ;; syntax->list returns #f if the syntax is not a list
          (if maybe-list (map strip maybe-list) stx)))
      (lambda (stx)
        (datum->syntax stx (transformer (strip stx)) stx))))
```

With this utility defined, the above macro becomes much easier to deal with:  

```racket
(define-syntax mylet
    (er-like-transformer
     (lambda (exp)
       (let ((vars  (map car (cadr exp)))
             (inits (map cadr (cadr exp)))
             (body  (cddr exp)))
         `((,(syntax lambda) ,vars ,@body)
           ,@inits)))))
 ```

 ...and this is almost identical to the explicit renaming version of the macro; for example, compare it with the sample code in the [MIT-Scheme manual](http://groups.csail.mit.edu/mac/projects/scheme/documentation/scheme_3.html#SEC49).  The only change is that `(rename 'lambda)` is replaced with `(syntax lambda)`.

Obviously, this is very close, but doesn't show intentional captures.  So I just grabbed the `loop` example from the same page, and did the same change — only this time I used `#'foo` instead of `(syntax foo)` (and I also changed the one-sided `if` to a `when`).  The resulting macro works fine:  

```racket
(define-syntax loop
    (er-like-transformer
     (lambda (x)
       (let ((body (cdr x)))
         `(,#'call-with-current-continuation
           (,#'lambda (exit)
            (,#'let ,#'f () ,@body (,#'f))))))))
  
  (define-syntax while
    (syntax-rules ()
      ((while test body ...)
       (loop (when (not test) (exit #f))
             body ...))))
  
  (let ((x 10))
    (while (> x 0)
      (printf "~s\n" x)
      (set! x (- x 1))))
```

This is pretty close to a library, and indeed, as I was writing this text I found [a post by Andre van Tonder](http://www.mail-archive.com/larceny-users@lists.ccs.neu.edu/msg00097.html) on the Larceny mailing list that uses a very similar approach and _does_ make a library out of it.  This is done by adding two arguments to the expected ER-transformation function — one is a `rename` function (since the above method uses `syntax` it is limited to immediate identifiers), and the other is always passed as `free-identifier=?`.  Such a compatibility library is, however, not the purpose of this post.

Finally, there is still a minor issue with this — PLT has an implicit `#%app` which is used wherever there are parentheses that stand for a function application — and in this code they are used unhygienically.  This is usually not a noticeable problem, and if it is, you can add explicit `#%app`s.  It might also be possible to find a more proper solution (e.g., use a hash table to keep track of lists that were disassembled by the client transformer), but at this point it might be better to just use the more natural `syntax-case` anyway.

<!-- more -->



* * *

turing tarpit? next corner right and there you are ;)

As you are quite aware, your "solution" changes the cost of macro-expansion to quadric. This is in constrast to linear cost for native syntax-case AND native ER-macros. As it is quite possible to implement syntax-case in terms of ER-transformers (or syntactic-closures) without loosing the linear complexity, many people claim that these two are more basic than syntax-case ... and therefore should be in the actual standard.

Whats your opinion eli?

— *derSlom, 24 May 2009*

* * *

to also add useful something besides nitpicking, I have been down that road as well: http://paste.lisp.org/display/41754

— *derSlom, 24 May 2009*

* * *

Although I don't know of any formal proofs of this, I don't believe that either ER or syntactic closures can express syntax case, nor can syntax case (in the original formulation) express ER.  

Also, ER really doesn't have a clear specification, so it's hard to make sure that an ER implementation that matches the original note works with actual code.

— *Sam TH, 24 May 2009*

* * *

derSlom: Yes, it is a demonstration of how to do ER-style for people who like that for the listed reason, and as a demonstration that syntax values don't require black magic, it's not a complete library.  Expanding it into a library will require adding a hash table to (1) unwrap each syntax value once and get the cost loss back, and (2) to associate each unwrapped list with the lost syntactic information (which, I think, will also solve the `#%app' problem).  BTW, unlike your code, I'm talking about a persistent hash, allowing you to add information to existing syntax values.

Sam: that sounds right, but I explicitly avoided that too.  I do think, though, that it's possible to express ER using something like the above -- even with #%app.

— *Eli Barzilay, 24 May 2009*

* * *

