
    Title:Dirty Looking Hygiene
    Date:2008-02-23T01:20:00.002-05:00
    Tags:

*posted by Eli Barzilay*

With the recent release of [Arc](http://arclanguage.com/), there has been some discussion over hygienic macros.  Yes, hygienic macros are usually very convenient, but they can become messy in some 'corner' cases.  People who learn about macros in Scheme usually start with `syntax-rules`, and being the limited tool that it is, they often get the impression that for advanced uses (like a macro that captures an identifier) you need to use `syntax-case` which is this "really obscure thing".

For example, say that we want to implement an `if` form that is similar to Arc's `if`.  This is pretty easy using `syntax-rules`:  


```racket
(define-syntax if*
    (syntax-rules ()
      [(if*) (void)]
      [(if* X) X]
      [(if* C X more ...) (if C X (if* more ...))]))
```

But more important than being easy to write: it is also easy to read. In fact, the nice thing about `syntax-rules` is that you write more or less the specification of your transformation.  Compare this to the specification of Arc's `if`, which appears in a comment before the definition of `ac-if` in "`ac.scm`": 

```racket
  ; (if) -> nil
  ; (if x) -> x
  ; (if t a ...) -> a
  ; (if nil a b) -> b
  ; (if nil a b c) -> (if b c)
```
(Except that the comment mixes up the syntactic specification and the semantic evaluation.)

As a side note, now that we have this definition, it is easy to construct a new language that is just like MzScheme, except for its `if` that behaves like the above:  

```racket
(module arc-like mzscheme
    (define-syntax if* ..._the above definition_...)
    (provide (all-from-except mzscheme if)
             (rename if* if)))
```

You can now write code that uses `"arc-like.scm"` as its language, using the new `if`.  There is no problem in accommodating two languages with two different `if`'s: the new form is compiled to the old one, and there is no confusion in which version you use in any module.

Back to the macro issue: as I said above, you run into problems if you want to capture names, right?  For example, if you want to implement Arc's `aif`.  The usual `syntax-case` solution is to construct an identifier that has the lexical context of the input syntax.  It's easy to abstract over all this -- I posted a [message](http://www.arclanguage.org/item?id=841) on the Arc forum showing how to define a [defmac](http://tmp.barzilay.org/defmac.ss) macro that has the simplicity of `syntax-rules` with the added convenience of specifying keywords and captured names.  This works for _some_ cases, but there are still some [subtle corner cases](http://download.plt-scheme.org/doc/372/html/mzscheme/mzscheme-Z-H-12.html#node_sec_12.2.1.1).

But there's a better solution in PLT Scheme, one that follows Paul Graham's intuition when he [says](http://www.arclanguage.org/item?id=2526): "captured symbols are kind of freaky." The basic idea is a change of perspective: instead of (unhygienically) binding individual occurrences of `it` whenever `aif` is used, you define `it` once as a thing in its own right -- a special context-dependent piece of syntax. Outside of an `aif` form, `it` has no meaning: we simply make it throw a syntax error.  Uses of `aif` provide a meaning for `it` by locally changing its meaning (its expansion) to something useful: the binding that holds the result of evaluating the condition expression.  ("Locally" means within a piece of syntax, so the new meaning is valid in a lexical-scope.)

In PLT Scheme, the "special context-dependent piece of syntax" are _syntax parameters_, and you change them locally with `syntax-parameterize`.

To continue the above example, here's how we make our `if*` anaphoric:

* require the `(lib "stxparam.ss" "mzlib")` library,

* define `it` as a syntax using `define-syntax-parameter`, and have it raise an error by default,

* bind a temporary variable to the result of evaluating the condition,

* wrap the positive branch with `syntax-parameterize`, using `make-rename-transformer`, which is a convenient way to make a macro that behaves like the new variable.

The implementation looks like this:

```racket
  (require (lib "stxparam.ss" "mzlib"))
  (define-syntax-parameter it
    (lambda (stx)
      (raise-syntax-error #f "can only be used inside `if'" stx)))
  (define-syntax if*
    (syntax-rules ()
      [(if*) (void)]
      [(if* X) X]
      [(if* C X more ...)
       (let ([b C])
         (if b
           (syntax-parameterize ([it (make-rename-transformer #'b)]) X)
           (if* more ...)))]))
```

The resulting macro does not break hygiene.  For example, `(let ([it 3]) (if #t it))` evaluates to `3`, because it shadows the global `it` that `if` changes.  This is a change from a real unhygienic macro -- but that's the whole point: we (the macro author) do not interfere with scopes in the user code.

Note that `(if 1 (if 2 it))` still evaluates to `2`, because the outer `if` does not really bind `it` -- it is not captured, just changed locally -- so the inner `if` changes it again.  Also, `(if #f it it)` raises the usual context error, since our macro changes `it` only in the positive branch.

<!-- more -->



* * *

The specification atop the macro reminds me of the quip that "any sufficiently well-commented lisp program contains an ML program in its comments" (:

— *Martin DeMello, 28 April 2008*

* * *

