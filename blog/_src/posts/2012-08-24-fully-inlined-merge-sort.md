
    Title:Fully Inlined Merge Sort
    Date:2012-08-24T16:52:00.001-04:00
    Tags:

*posted by Neil Toronto*

While writing the code for the [triangular distribution](http://en.wikipedia.org/wiki/Triangular_distribution) in the upcoming math library, I found that I needed a function that sorts exactly three numbers. This kind of code is annoying to write and to get right. But it comes up rarely enough, and it seems simple enough, that I’ve never felt like making a library function for it.

But what if I wrote a macro that generated code to sort n numbers very quickly, where n is known at expansion time, but the numbers themselves aren’t? I think I could justify putting that in a library.

Here’s code that correctly sorts three numbers a, b and c: 

```racket
(if (< b c)
    (if (< a b)
        (values a b c)
        (if (< a c)
            (values b a c)
            (values b c a)))
    (if (< a c)
        (values a c b)
        (if (< a b)
            (values c a b)
            (values c b a))))

```


It’s an if tree. Notice that there are 6 leaf expressions, for the 3! = 6 possible permutations. Also, it never compares more than it has to. It’s optimal.The optimality came from my reasoning about transitivity. For example, only two comparisons are needed before returning (values a b c). I knew that both (< a b) and (< b c), so (< a b c) must be true by transitivity.

It would be nice if the macro generated optimal code by explicitly reasoning about transitivity, or as an emergent property of the sorting algorithm it uses.

We’ll write a macro that does the latter, by generating a fully inlined merge sort.

[Edit: The final inline sort macro is [here](https://gist.github.com/3456604).]

Runtime Merge Sort
---

Start with a simple, runtime merge sort: 

```racket
(define (merge as bs)
  (match* (as bs)
    [((list) bs)  bs]
    [(as (list))  as]
    [((list a as ...) (list b bs ...))
     (if (< a b)
         (cons a (merge as (cons b bs)))
         (cons b (merge (cons a as) bs)))]))
 
(define (merge-sort vs)
  (match vs
    [(list)  vs]
    [(list a)  vs]
    [_  (define-values (lvs rvs)
          (split-at vs (quotient (length vs) 2)))
        (merge (merge-sort lvs) (merge-sort rvs))]))

```

Example:

```racket
> (merge-sort '(5 1 2 5 10))
'(1 2 5 5 10)
```

To make a macro out of merge sort, we need to change two things. The most obvious is that it has to return syntax for an if instead of evaluating it. That’s easy for a novice macrologist: change the functions to operate on syntax, stick a syntax-quasiquote in front of the if, and unquote the bits inside that get evaluated at expansion time.

But if we did only that, we’d end up with expanded code like this: 

```racket
(if (< a b)
    (cons a (if ...))
    (cons b (if ...)))
```

It would be slow because cons allocates. We want the code to be fast.So the other change is to move the conses inside the ifs, and evaluate them at expansion time. We can then construct a values expression out of the resulting list.

Accumulator-Passing Style Won’t Work
---

 Novice functional programmers should know that accumulator-passing style (APS) moves conses inward. For example, this “add 1 to each element” function:

 ```racket
 (define (list-add1 vs)
  (match vs
    [(list)  (list)]
    [(list v vs ...)
     (cons (add1 v) (list-add1 vs))]))
```


becomes this after conversion to APS: 

```racket
(define (list-add1/acc vs [acc (list)])
  (match vs
    [(list)  (reverse acc)]
    [(list v vs ...)
     (list-add1/acc vs (cons (add1 v) acc))]))
```

Now cons is where we want it: inside the recursive call, instead of in tail position. The problem is that APS doesn’t work on tree-shaped recursion.

Continuation-Passing Style Does Work
---

In continuation-passing style (CPS), we pass a continuation k—i.e. “what happens next”— instead of an accumulator. The functions call k instead of returning values. For example: 

```racket
(define (list-add1/k vs [k identity])
  (match vs
    [(list)  (k (list))]
    [(list v vs ...)
     (list-add1/k vs (λ (vs) (k (cons (add1 v) vs))))]))
```

If we want, we can pass something besides identity as the base-case continuation: 

```racket
> (list-add1/k '(1 2 3 4) (λ (vs) (apply values vs)))
2
3
4
5
```

CPS turns every call into a tail call, so it moves conses inward even with tree-shaped recursion. As a demonstration, here’s a CPSed merge sort:

```racket
(define (merge/k as bs k)
  (match* (as bs)
    [((list) bs)  (k bs)]
    [(as (list))  (k as)]
    [((list a as ...) (list b bs ...))
     (if (< a b)
         (merge/k as (cons b bs) (λ (vs) (k (cons a vs))))
         (merge/k (cons a as) bs (λ (vs) (k (cons b vs)))))]))
 
(define (merge-sort/k vs k)
  (match vs
    [(list)  (k vs)]
    [(list a)  (k vs)]
    [_  (define-values (lvs rvs)
          (split-at vs (quotient (length vs) 2)))
        (merge-sort/k
         lvs (λ (lvs)
               (merge-sort/k
                rvs (λ (rvs)
                      (merge/k lvs rvs k)))))]))
```

You can read the last expression in merge-sort/k as, “Sort lvs. Then, with the sorted lvs, sort rvs. Then, with the sorted rvs, merge lvs and rvs.”
Example:

```racket
> (merge-sort/k '(5 2 3 1) (λ (vs) (apply values vs)))
1
2
3
5
```


The Inline Sort Macro
---

When we macro-ize the CPSed merge sort, we’ll turn the continuations into expansion-time functions. So not only will macro-ized CPS move conses inward, it’ll apply them all at expansion time!

We’ll do it in three parts: the user-facing macro, the inline merge function, and the inline sort function.

The User-Facing Macro
---

Let’s put a nice face on inline sorting: 

```racket
(define-syntax (inline-sort stx)
  (syntax-case stx ()
    [(_ lst ...)
     (with-syntax ([(vs ...)
                    (generate-temporaries #'(lst ...))])
       #`(let ([vs lst] ...)
           #,(inline-sort/k #'(vs ...)
                            (λ (vs) #`(values #,@vs)))))]))
```

This macro does two things. First, it names the values to be sorted, so they don’t get re-evaluated every time they’re compared. Second, it calls inline-sort/k with a base-case continuation that converts syntax lists to values expressions.Note that the call #,(inline-sort/k ...) happens at expansion time, and that the continuation (λ (vs) ...) it passes is an expansion-time value.

The Inline Merge Function
---

Changing merge/k to operate on syntax at expansion time is as straightforward as possible: 

```racket
(define-for-syntax (inline-merge/k as bs k)
  (syntax-case (list as bs) ()
    [(() bs)  (k #'bs)]
    [(as ())  (k #'as)]
    [((a as ...) (b bs ...))
     #`(if (< a b)
           #,(inline-merge/k #'(as ...) #'(b bs ...)
                             (λ (vs) (k (cons #'a vs))))
           #,(inline-merge/k #'(a as ...) #'(bs ...)
                             (λ (vs) (k (cons #'b vs)))))]))

```


The only substantial changes are the quasiquotes and unquotes, and using syntax-case to destructure syntax lists instead of using match to destructure lists. Again, note that the recursive calls #,(inline-merge/k ...) in each if branch happen at expansion time, and that their continuations are expansion-time values.We can see what kind of code merge/k returns by applying it at expansion time: 

```racket
> (begin-for-syntax
    (print
     (syntax->datum
      (inline-merge/k #'(a) #'(b c) (λ (vs) vs)))))
'(if (< a b) (a b c) (if (< a c) (b a c) (b c a)))
```

The syntax list #'(b c) is assumed already sorted, meaning (< b c) at runtime. Therefore, if (< a b) at runtime, by transitivity, (< a b c) at runtime, so the merge generates #'(a b c). In other words, inline-merge/k’s assumption that its arguments are sorted is equivalent to reasoning about transitivity.

The Inline Sort Function
---

Lastly, the divide-and-conquer part: 

```racket
(require (for-syntax racket/list))  ; for list functions
 
(define-for-syntax (inline-sort/k vs k)
  (syntax-case vs ()
    [()  (k vs)]
    [(a)  (k vs)]
    [_  (let ([vs  (if (list? vs) vs (syntax->list vs))])
          (define-values (lvs rvs)
            (split-at vs (quotient (length vs) 2)))
          (inline-sort/k
           lvs (λ (lvs)
                 (inline-sort/k
                  rvs (λ (rvs)
                        (inline-merge/k lvs rvs k))))))]))
```

This is changed similarly to merge/k. The only new change is using syntax->list to convert syntax to lists so we can use the functions length and split-at.
Example:

```racket
> (inline-sort 5 2 3 1)
1
2
3
5
```

Of course, the result of evaluating an inline-sort doesn’t tell the whole story. Let’s fire up the macro stepper and see what (inline-sort 5 2 3) expands to. Copying from the macro stepper window and renaming temp variables, we get 

```racket
(let ([a 5] [b 2] [c 3])
  (if (< b c)
      (if (< a b)
          (values a b c)
          (if (< a c)
              (values b a c)
              (values b c a)))
      (if (< a c)
          (values a c b)
          (if (< a b)
              (values c a b)
              (values c b a)))))

```

It’s an if tree. Notice that there are 6 leaf expressions, for the 3! = 6 possible permutations. Also, it never compares more than it has to. It’s optimal.

Inline Sort Properties
---

Inherited from the merge sort, the inline sort has the following properties (assuming a length-n list): 

* Time optimality: The depth of the if tree is O(n log(n)).

* Size optimality: The number of leaves is exactly n!.
The term “size optimality” is misleading, because that’s still a lot of code. I’ve seriously considered requiring any user of this macro to state how many permutations there are for the number of values they’re sorting. They’d have to prove to the macro that they know how much code they’re asking it to generate.Inline sort is wicked fast, as we should expect: 

```racket
> (define vs '(5 2 3 1))
> (match-define (list a b c d) vs)
> (for ([_  (in-range 5)])
    (time (for ([_  (in-range 1000000)])
            (match-let ([(list a b c d)  (sort vs <)])
              (void)))))
cpu time: 550 real time: 540 gc time: 30
cpu time: 510 real time: 518 gc time: 0
cpu time: 520 real time: 517 gc time: 20
cpu time: 520 real time: 517 gc time: 0
cpu time: 510 real time: 516 gc time: 10
> (for ([_  (in-range 5)])
    (time (for ([_  (in-range 1000000)])
            (let-values ([(a b c d)  (inline-sort a b c d)])
              (void)))))
cpu time: 20 real time: 28 gc time: 0
cpu time: 30 real time: 27 gc time: 0
cpu time: 30 real time: 27 gc time: 0
cpu time: 30 real time: 27 gc time: 0
cpu time: 20 real time: 27 gc time: 0

```

So about 20 times faster than sort on a length-4 list.I use it in Typed Racket, on floating-point numbers. Typed Racket’s optimizer replaces `<` with `unsafe-fl<`. This tells the compiler that the elements are floats, so it can keep them stack-allocated, which reduces allocations further. In all, for my particular use of inline-sort, it’s over 50 times faster than sort.And using it is a heckuvalot easier than writing an if tree and reasoning about transitivity every time I need to sort a handful of floats.

Conclusion
--

 Writing macros in expansion-time CPS to fully inline a recursive function works out beautifully. I suspect that it will work on any recursive, polymorphic function whose well-foundedness follows only from the structure of the input data.Also, it can generate a metric truckload of code.Interesting note: I originally wrote inline-sort using only syntax-rules, passing the names of higher-order macros to other macros as continuations. Sorting a five-element list took almost 19000 expansion steps, which is ridiculously inefficient even for a 120-leaf if tree.

<!-- more -->



* * *

Looks like a functional pearl to me!

— *Matt, 24 August 2012*

* * *

As i remember, sorting networks macro from `Let over Lambda` book, solves the same problem differently.

— *kmmbvnr, 24 August 2012*

* * *

