
    Title:Writing `syntax-case` Macros
    Date:2011-04-04T14:00:00.001-04:00
    Tags:

*posted by Eli Barzilay*

Disclaimer: This is not really a tutorial on macros, it's more of a quick introduction to using Racket's `syntax-case`-based macros for people who are familiar with symbolic macros and miss their “simplicity”.  It's also not comprehensive or thorough or complete, it's just intended to provide a rough quick overview of how to write macros.  It was originally posted on comp.lang.scheme in a thread called “Idiot's guide to Scheme macros”, but I avoided that title here, since it's not a general purpose guide.  (Also, it's yet another attempt to dispel the irrational “macrophobia” some people have when it gets to hygienic macros, leading them back to using `defmacro` with all its problems.)

The main idea with Racket's macro system (and with other `syntax-case` systems) is that macros are syntax-to-syntax functions, just like the case of `defmacro`, except that instead of raw S-expressions you're dealing with syntax objects.  This becomes very noticeable when identifiers are handled: instead of dealing with plain symbols, you're dealing with these syntax values (called “identifiers” in this case) that are essentially a symbol and _some opaque information_ that represents the lexical scope for its source.  In several `syntax-case` systems this is the only difference from `defmacro` macros, but in the Racket case this applies to everything — identifiers, numbers, other immediate constants, and even function applications, etc — they are all the same S-expression values that you're used to, except wrapped with additional information.  Another thing that is unique to Racket is the extra information: in addition to the opaque lexical context, there is also source information and arbitrary properties (there are also certificates, but that's ignorable for this text).

With this in mind, explaining the rest is not too difficult:

* `(syntax-source stx)`, `(syntax-position stx)`, `(syntax-line stx)`, `(syntax-column stx)` — retrieve parts of the source location information.

* `(syntax-e stx)` — takes a syntax value and returns the value it “wraps”.  For example, if `stx` is an identifier you'd get a symbol, and if it's a number you'd get the number.  If it's a simple parenthesized form, you'd get a list of syntax values for the subforms.  Note that the list can be improper, with the last element being a syntax object that contains a proper list.  (But the list will actually be improper if the original syntax was a dotted list.)

* `(syntax->datum stx)` — takes a syntax value and returns the plain S-expression that it holds.  This is done by recursive uses of `syntax-e`.  (It would be a simple definition that does what you'd think it should do.)

* `(syntax->list stx)` — sometimes you want to pull out the list of syntax values from a given parenthesized syntax, but `syntax-e` does too little (can still return an improper list) and `syntax->datum` does too much (gives you back raw S-expressions).  `syntax->list` is a utility function that uses `syntax-e` as many times as needed to get back a proper list of syntax values.  If that's not possible (if the input syntax was not a proper list), it returns `#f`, so it serves as a predicate too.

* `(syntax-property stx prop)` — returns the given property value from stx, if any, and `#f` if none.  For example, try 

```racket
(syntax-property #'[foo] 'paren-shape)
```

(The `#'` is similar to a quote, but for syntax values — I'll get to that later on.)

* Note that there is _no_ accessor for the opaque lexical scope, and as you'll see next, you don't need one.

* To create a piece of syntax you use `datum->syntax`, and you give it an S-expression which will be the “contents” of the resulting syntax object.  (The input can contain syntax values, which are left as is.)  But when you do that you need to give it the other bits — including the lexical context thing, which you have no access to.  The way that's done is:

```racket
(datum->syntax context-stx input-sexpr) 
```

This returns a syntax value that wraps the `input-sexpr` value, using the lexical scope from `context-stx`.  A common way to “break hygiene” and create a binding that is visible to the macro user's code is: 

```racket
(datum->syntax stx 'foo)
```

where `stx` is some syntax value that you get from the user input to the macro.  It returns a `foo` identifier that has the same lexical context information as `stx`, so it's as if it came from there.

Note that there is actually another optional argument that specifies the source (either using another syntax object, or as an explicit list), and another for copying the properties from — so an alternative to the above would be:

```racket
(datum->syntax stx 'foo stx stx)
```

which also makes the source information and the properties be the same as those of `stx` (for example, this can matter in case of syntax errors).


* There is also `(quote-syntax blah)` which creates a quoted syntax, with its lexical source from the place it appears.

* Finally, `define-syntax` does the magic of tying a name with a transformer function.

And that's almost everything that you need in order to write hygienic (and non-hygienic) macros.  Very inconveniently.

For example, here's a simple `while` macro (use this in a file that starts with `#lang racket`):

```racket
(define-syntax (while stx)
  (define subs (syntax->list stx))
  (datum->syntax
   stx
   `(let loop ()
      (when ,(cadr subs)
        ,@(cddr subs)
        (loop)))
   stx))
```

which breaks like this:

```racket
(define x 2)
(let ([let 5])
  (while (< x 10)
    (printf "x = ~s\n" x)
    (set! x (add1 x))))
```

The problem is that all of those quoted names are getting the context of the user input, which is not the right thing (it's close to a `defmacro`).  To fix this, you need to `quote-syntax` all of these identifiers, so they'll have the macro source instead of the input source:

```racket
(define-syntax (while stx)
  (define subs (syntax->list stx))
  (datum->syntax
   stx
   `(,(quote-syntax let) ,(quote-syntax loop) ()
     (,(quote-syntax when) ,(cadr subs)
      ,@(cddr subs)
      (,(quote-syntax loop))))
   stx))
```

But that's clearly insane...  More than being tedious, it's still incorrect since all of those function application parens will have the user's lexical context (Racket has a special implicit `#%app` macro that gets used in all function applications, and in this case the context of this application will make it unhygienic).  Instead of doing this, a better approach would be to create the resulting syntax with the lexical context of the macro source by changing just that argument:

```racket
(define-syntax (while stx)
  (define subs (syntax->list stx))
  (datum->syntax
   (quote-syntax here)
   `(let loop ()
      (when ,(cadr subs)
        ,@(cddr subs)
        (loop)))
   stx))
```

And that's simple again, and works fine now.

The problem is that it's tedious wrt to deconstructing the input (which happens to be trivial in this case), and wrt slapping together an output value — and that's where `syntax-case` comes in.  It addresses the both by using pattern matching, where identifiers in patterns are bound as “syntax patterns”.  A new form is added — `syntax` — which is similar to a `quote`, except that (a) it actually quotes things similarly to `quote-syntax`, with the lexical context of the `syntax` form; and (b) pattern variables are substituted with what they matched.  With this, the above macro becomes much easier:

```racket
(define-syntax (while stx)
  (syntax-case stx ()
    [(_ test body ...)
     (syntax (let loop ()
               (when test
                 body ...
                 (loop))))]))
```

The first line specifies that you want to match the `stx` input syntax, and that you have no “keywords” (in the same sense as in `syntax-rules`).  The second line is the pattern that is matched against this input — with two pattern variables that match the second subexpression and the sequence of expressions from the third and on.  (The first subexpression is matched against `_` which is a wild-card that matches anything without binding a pattern variable — the head part is often not needed, since it's just the macro name.)  The last line is the output, specified using `syntax`, which means that it's very similar to the previous version where everything is given the lexical context of the macro and the two pattern variables are replaced with the two matches (so `body` gets spliced into the resulting syntax).

Now, say that you want an unhygienic user-visible piece of syntax.  For example, bind the always entertaining `it` thing to the test result.  This:

```racket
(define-syntax (while stx)
  (syntax-case stx ()
    [(_ test body ...)
     (syntax (let loop ()
               (let ([it test])
                 (when it
                   body ...
                   (loop)))))]))
```

won't work because `it` has the macro source — it's hygienic and therefore not visible.  Instead, you need to use `datum->syntax` with the user syntax:

```racket
(define-syntax (while stx)
  (syntax-case stx ()
    [(_ test body ...)
     (let ([it (datum->syntax stx 'it)])
       (syntax (let loop ()
                 (let ([it test])
                   (when it
                     body ...
                     (loop))))))]))
```

But this doesn't really work since `it` needs to be bound as a pattern variable rather than a plain binding.  `syntax-case` can be used here again: `(syntax-case <name> () [foo <body>])` will match `foo` against the `<name>` syntax, and if it's a name then it will be bound as a pattern variable in the `<body>`.


```racket
(define-syntax (while stx)
  (syntax-case stx ()
    [(_ test body ...)
     (syntax-case (datum->syntax stx 'it) ()
       [it (syntax (let loop ()
                     (let ([it test])
                       (when it
                         body ...
                         (loop)))))])]))
```

Note that since `it` is a pattern variable, it doesn't need to be unquoted — `syntax` will do that. 

Finally, there are some more conveniences.  First, `with-syntax` is a macro that binds pattern variables (by a similar translation to `syntax-case`):

```racket
(define-syntax (while stx)
  (syntax-case stx ()
    [(_ test body ...)
     (with-syntax ([it (datum->syntax stx 'it)])
       (syntax (let loop ()
                 (let ([it test])
                   (when it
                     body ...
                     (loop))))))]))
```

and there's the `#'` reader macro for `syntax`:

```racket
(define-syntax (while stx)
  (syntax-case stx ()
    [(_ test body ...)
     (with-syntax ([it (datum->syntax stx 'it)])
       #'(let loop ()
           (let ([it test])
             (when it
               body ...
               (loop)))))]))
```

 and there are also `` #` `` and `#,` and `#,@` which are implemented by translating them to uses of `with-syntax`.

 Note that the last example uses the lexical context of the whole form for the new identifier, but that's not only the option.  You could use any other part of the macro input — for example, you could use the macro keyword:

```racket
(define-syntax (while stx)
(syntax-case stx ()
  [(hd test body ...) ; need the head now
   (with-syntax ([it (datum->syntax #'hd 'it)])
     ... same ...)]))
```


 or the test expression (use `#'test`).  Each of these choices has subtle differences that are especially important when you're composing macros (for example, using a second macro that _expands_ to a `while`, where the test expression comes from that macro rather than the user code).  Demonstrating these things is a popular way to pass the time in some circles, but I'll avoid it here.  In fact, a great way to avoid this whole thing altogether is not create unhygienic bindings in the first place.  It sounds like doing so excludes cases where you _really_ want to have a new binding visible in user code, but Racket provides “syntax parameters” that can be used more conveniently (and less confusingly) — see [an earlier post](../../2008/02/dirty-looking-hygiene.html) for a description of that.  As a side note, these options are a good hint that a hygienic macro system is more expressive than a symbolic `defmacro` system, where no such choices exist.  Creating such macros using `defmacro` can appear easier simply because of this lack of choice — in the same way that CPP-style string-based macros are “simpler” than `defmacro` since they're less expressive (just appending lexical tokens, no structural information).

 There are other important aspects of the Racket macro system that are not covered here.  The most obvious of them is worth mentioning here: Racket separates the “runtime phase” from the “syntax phase”.  For example, if you want to try these examples with “`#lang racket/base`”, you'll need to add `(require (for-syntax racket/base))` since the `racket/base` language doesn't have a full language in its syntax phase.

 Roughly speaking, this makes sure that source code is deterministically compilable by having each level live in its own world, limiting macros to deal only with the input syntax only and not runtime values.  (For example, a CLOS implementation in this system cannot check the value of an identifier bound to a class to determine how some macro should expand.)  This results in reliable compilations that do not depend on how things were loaded, or whatever happened on the REPL.

 The important bottom line here is that you get to write macros with the full language available — and phase separation means that Racket is explicitly designed to make running code at the macro level and using it by the compiler as robust as possible, so you don't have to worry about using any complex system as part of your macro.  You just need to keep in mind that the macro world is completely separate from the runtime, and the direct benefit of not worrying about weird interactions with compilation and file loading orders.