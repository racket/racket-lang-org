    Title: Type Tailoring
    Date: 2017-04-18T16:35:47
    Tags: 

*posted by Ben Greenman*

Type tailoring is a technique for adding _domain-specific_ type checkers to a
 typed host language.
Using the [_Type Systems as Macros_][1] approach to building typed languages,
 implementing type tailoring in Typed Racket is straightforward.
Any library can apply the core idea, and you can try programming with
 type tailorings by downloading the [`trivial`][9] package
 (requires Racket v6.4 or later).

<!-- more -->

## Background: Programming the Type Elaborator

Many typed languages include both a type checker and a _type elaborator_.
The type elaborator translates source code to an [explicitly typed][4]
representation for the type checker to validate.
Normally, programmers cannot extend the behavior of the type elaborator
 without modifying the compiler.

In [Typed Racket][6], the Racket macro expander serves as a type elaborator.
A fully-expanded Typed Racket program is valid input to the type checker.
This design makes Typed Racket _macro-extensible_; programmers can write their
 own macros to [grow][5] the syntax of the language.
More on this in a minute.


Recently, [Stephen Chang](https://ccs.neu.edu/home/stchang) observed
 that individual macros can implement the same pipeline as Typed Racket ---
 macros can expand their sub-terms and perform static checks on the results.
Such macros are essentially type rules, and
 a well-designed library of such macros is essentially a type system.
Hence [_Type Systems as Macros_][1]: a library for building
 typed, domain-specific languages (DSLs) using the Racket macro system.


_Type tailoring_ is programming a language's type elaborator to implement
 _domain-specific typing rules_ on top of an existing type system.
Any language that lets programmers extend the behavior of its type elaborator
 supports type tailoring.
In particular, Typed Racket programmers can use the
 _Type Systems as Macros_ technique to implement tailored APIs as macros.
These macros can statically check properties of source code, communicate
 the results of their checks to other macros, and expand to type annotated
 code for the Typed Racket type checker.

As we will see, type tailoring can even extend a "conventional" typed language
 with static checks that would otherwise require dependent types.


## Case Study: Regular Expressions
### `regexp-match`[racket] in Racket

Racket's [regular expression library](http://docs.racket-lang.org/reference/regexp.html)
 will match a string, path, byte string, or input port
 against a given pattern.

```racket
  $ racket
  > (regexp-match "r" "racket")
  ; '("r")
  > (regexp-match "z" "racket")
  ; #f
  > (regexp-match "r|z" "racket")
  ; '("r")
  > (with-input-from-string "hello world"
      (λ () (regexp-match "hello (.*)$" (current-input-port))))
  ; '(#"hello world" #"world")
```
<br/>

The result of matching a pattern against an input source is `#f`[racket] if
 no part of the input matched the pattern.
Otherwise, the result is a pair containing:

- the subsequence of the input that matched the pattern, and
- a list of subsequences that were captured by parenthesized sub-patterns in
   the pattern (_capture groups_).

Note that a capture group may fail to capture any subsequence.
If this happens, the list of subsequences contains `#f`[racket] in the
 corresponding position.

```racket
  $ racket
  > (regexp-match "(r)|(z)" "racket")
  ; '("r" "r" #f)
```
<br />

### `regexp-match`[racket] in Typed Racket

Typed Racket includes a type signature for `regexp-match`[racket] that accepts
 the same variety of input sources, but conservatively rejects some "correct"
 uses of the output.


```racket
  $ racket -I typed/racket
  > (regexp-match "(a*)b" "aaabbb")
  ; - : (U False (Pairof String (Listof (U False String))))
  ; '("aaab" "aaa")

  > (let ([m (regexp-match "(a*)b" "aaabbb")])
    (when m
      (string-length (second m))))
  ; Type Checker: Polymorphic function `second' could not be applied to arguments:
  ;   ....
  ; Arguments: (Pairof String (Listof (U False String)))
  ; Expected result: String
  ;   in: (second m)
```
<br/>

The issue is that `string-length`[racket] expects a string, but Typed Racket
 reasons that the call to `second`[racket] might return a string or false.
One solution is to guard the value `(second m)`[racket] with a dynamic check.
Another is to apply type tailoring to `regexp-match`[racket]!


### Type Tailored `regexp-match`[racket]

We can implement a type-tailored interface to `regexp-match`[racket]
 with a macro that:

- checks whether its first argument is a string literal
- counts the number of matched parentheses in the string literal
- expands to _code designed to convince Typed Racket_ that the result is a list
   with the proper number of arguments.

The third point is crucial, and demonstrates how type tailoring can translate
 program properties that are _implicit_ in the source code to facts that
 the type checker can understand.

Here is the macro:

```racket
(define-syntax (smart-match stx)
  (syntax-parse stx
   [(_ pattern:str other-args ...)
    (define num-groups (count-groups (syntax-e #`pattern)))
    #`(let ([m (regexp-match pattern other-args ...)])
        (if m
          (list
            (car m)
            #,@(for/list ([i (in-range num-groups)])
                 #`(or (list-ref m (+ 1 #,i))
                       (error 'smart-match))))
          m))]
   [(_ args ...)
    #`(regexp-match args ...)]
   [_
    #`regexp-match]))
```
<br/>

> Scroll to the bottom of this post for an implementation of `count-groups`[racket].


Quick guide to reading the macro:

- `syntax-parse`[racket] is a pattern-matcher, the three clauses match possible uses of `smart-match`[racket]
- <tt>#\`</tt> is a constructor for [syntax objects](http://docs.racket-lang.org/reference/syntax-model.html#%28tech._syntax._object%29)
- the `#,@(for/list ....)` term escapes to Racket's `for/list`[racket]
   combinator to build a list of syntax objects, then splices the generated
   syntax into the program --- as if the programmer had written out a list
   with `num-groups`[racket] elements

And here is a "client side" use of the macro:

```racket
  (let ([m (smart-match "(a*)b" "aaabbb")])
    (when m
      (string-length (second m))))
  ; 3
```
<br/>

It's exactly the result we want, and one renaming away from the
 code we want to write.


### Improving `smart-match`[racket]

Our `smart-match`[racket] has a few obvious limitations.
First, it reports an error if a capture group fails to capture a string.
Second, it fails to tailor calls where the pattern is a byte string
 or regular expression literal.
In the cases, `smart-match`[racket] defaults to Typed Racket's
 `regexp-match`[racket].
Third, it fails to tailor calls where the pattern is a variable.

The first problem is easy to fix by changing `count-groups`[racket] to return
 a list of `num-groups`[racket] booleans encoding whether each capture group
 definitely or maybe captures a string when the overall match is successful.
The second problem is also just a matter of engineering.

For the third problem, we can use the _Type Systems as Macros_ technique
 of using the macro expander to replace bound occurrences of a variable
 with type-annotated variables.
The domain-specific type we attach describes the capture groups; for example,
 given the variable declaration:

```racket
  (define x #rx"([a-z]+)@gmail\\.com")
```
<br/>

 the type annotation is `'(#t)`[racket] because it contains one capture group
 that is certain to capture a string when used in a successful
 `regexp-match`[racket].
This annotation can be attached to occurrences of `x`[racket] at expansion-time
 as a [syntax property](http://docs.racket-lang.org/reference/stxprops.html#%28tech._syntax._property%29)
 for `smart-match`[racket] to extract.

>  The [`trivial`][3] package includes a tailored
>   [`regexp-match`[racket]](http://docs.racket-lang.org/trivial/index.html#%28def._%28%28lib._trivial%2Fregexp..rkt%29._regexp-match%29%29)
>   that addresses all three issues.
>  Note: the fix for the third issue changes the expansion-time meaning of `define`[racket]
>   to infer and propagate domain-specific types.


### Practical Benefits

Now that we've thoroughly criticized `smart-match`[racket], it's worth pointing
 out its strengths.

- `smart-match`[racket] does **not** re-implement regular expression matching.
   It re-uses Racket's existing, correct, and performant `regexp-match`[racket].
- `smart-match`[racket] supports the existing syntax for regular expressions.
- `smart-match`[racket] validates some programs that Typed Racket conservatively
   rejects.
- `smart-match`[racket] rejects some programs that Typed Racket would accept,
   yet evaluate to runtime errors --- for instance, `(second (regexp-match "r" "racket"))`[racket].

Lastly, the use of `list-ref`[racket] exploits its conservative type to improve
 the conservative type of `regexp-match`[racket].
Whether you find this use clever or brittle, it is a proof to Typed Racket that
 does not resort to type casts.

> Implementing general _type checker plugins_ that can convince the host
>  type system of their correctness is a hard problem.
> I know of three high-quality type checker plugins for Haskell, and all three
>  assert their correctness to GHC
>  ([`type-nat-solver`](https://github.com/yav/type-nat-solver),
>   [`natnormalize`](https://github.com/christiaanb/ghc-typelits-natnormalise), and
>   [`uom-plugin`](https://github.com/adamgundry/uom-plugin)).


## Comparison: Dependently Typed Haskell

At this year's POPL, [Stephanie Weirich](http://www.cis.upenn.edu/~sweirich)
gave a keynote about [_The Influence of Dependent Types_][7] on the Haskell
type system.
After a brief introduction, her fifth slide gave motivation for dependently
 typed Haskell: _domain-specific type checkers_.

<img src="/img/why-dependent-haskell.png"
     alt="Why Dependent Haskell? Domain-specific type checkers"
     border="4"
     style="width: 40%" />

The rest of the talk was a code walk of a
 [dependently typed regular expression matcher][8]!
Stephanie's regular expression syntax supports Python-style named capture
groups, and a successful match returns a record with group names as keys.
These keys are reflected in the return type.
Of course the point of the talk was not regular expressions, but rather
 how some impressive GHC plugins compose to solve a practical problem.

What I want to point out, however, is that if the goal is "domain-specific
 type checkers", type tailoring is a more direct solution.
So if you are a programmer using Haskell or OCaml or Racket or Scala or
 Clojure or Java or any other language with a decent syntax extension system,
 you don't need to wait for "dependently typed `X`" to add more static checking
 to your library.
Write a macro!


## More Type Tailoring

There is a full implementation of type-tailored `regexp-match`[racket] on the
 [Racket package server](https://pkgd.racket-lang.org).
Try it out by installing the [trivial-pkg][9] package:

```bash
    $ raco pkg install trivial
```
<br/>

The package also includes tailorings for:

- [format strings](http://docs.racket-lang.org/trivial/index.html#%28part._.Format_.Strings%29)
- fixed-length [lists](http://docs.racket-lang.org/trivial/index.html#%28part._.List_.Operations%29) and [vectors](http://docs.racket-lang.org/trivial/index.html#%28part._.Vector_.Operations%29)
- [_N_-ary functions](http://docs.racket-lang.org/trivial/index.html#%28part._.Functions%29)
- [constant-folding arithmetic](http://docs.racket-lang.org/trivial/index.html#%28part._.Integer_.Arithmetic%29)

as well as syntax for defining new tailorings.

For further reading on type tailoring, and a sketch of how to prove the
 soundness of a tailoring, we have a draft paper:

- <http://hdl.handle.net/2047/D20324606>

The draft also reports on a small evaluation of our tailored
 `regexp-match`[racket] on existing code.
We searched the Racket distribution and libraries for uses of `regexp-match`[racket]
 and found over 300 in untyped code.
We then converted these uses to Typed Racket.
After converting, most uses did not compile using Typed Racket's `regexp-match`[racket],
 but swapping in the tailored `regexp-match`[racket] (a 1-line change) removed
 the type errors in over 250 cases.


### Appendix: `count-groups`[racket]

Here is a simple implementation for `count-groups`[racket].
[This file](http://docs.racket-lang.org/trivial/index.html#%28part._.Integer_.Arithmetic%29)
contains a more robust implementation (look for `parse-groups`[racket]).

```racket
(define-for-syntax (count-groups str)
  (define last-open-paren (box #f))
  (define num-groups
    (for/fold ([num-groups 0])
              ([c (in-string str)]
               [i (in-naturals)])
      (case c
       [(#\()
        (set-box! last-open-paren i)
        num-groups]
       [(#\))
        (if (unbox last-open-paren)
          (begin (set-box! last-open-paren #f)
                 (+ 1 num-groups))
          (error 'smart-match "unmatched ')' at position ~a in '~a'" i str))]
       [else
        num-groups])))
  (if (unbox last-open-paren)
    (error 'smart-match "unmatched '(' at position ~a in '~a'" (unbox last-open-paren) str)
    num-groups))
```
<br/>


[1]: http://www.ccs.neu.edu/home/stchang/popl2017/
[2]: http://www.ccs.neu.edu/home/types/resources/type-tailoring.pdf
[3]: http://docs.racket-lang.org/trivial/index.html
[4]: http://www.lfcs.inf.ed.ac.uk/reports/87/ECS-LFCS-87-42/ECS-LFCS-87-42.pdf
[5]: https://www.youtube.com/watch?v=_ahvzDzKdB0
[6]: http://docs.racket-lang.org/ts-reference/index.html
[7]: http://www.cis.upenn.edu/~sweirich/talks/popl17.pdf
[8]: https://github.com/sweirich/dth/tree/master/popl17
[9]: https://pkgd.racket-lang.org/pkgn/package/trivial
