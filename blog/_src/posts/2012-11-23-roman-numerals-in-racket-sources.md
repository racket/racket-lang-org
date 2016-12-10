
    Title:Roman Numerals in Racket Sources
    Date:2012-11-23T11:40:00.001-05:00
    Tags:

*posted by Shriram Krishnamurthi*

The other day, while discussing Church numerals in class, I pointed out that Racket could support Roman numeral in source programs. The essence of the idea is that, whenever an unbound identifier matches the syntax of a Roman numeral, it is automatically converted into the corresponding number.


The implementation of this is [here](https://github.com/shriram/roman-numerals). The [test client](https://github.com/shriram/roman-numerals/blob/master/test-client.rkt) best illustrates this in action.  For instance, here is a valid test case:

```racket
(define (square x) (* x x))
(check-equal? (square X) C)
```

The essence of the implementation is just this macro:

```racket
(define-syntax (handle-id stx)
  (syntax-case stx ()
    [(_ . any)
     (let ([str (symbol->string (syntax->datum #'any))])
       (if (roman-string? str)
           (with-syntax [(n (datum->syntax stx (roman->number str)))]
             #'(#%datum . n))
           #'(#%top . any)))]))
```

<!-- more -->



* * *

I(define MMMMM "that is especially delicious!")



— *steck, 23 November 2012*

* * *

It's a great setup for counters, too:
(do ((i i …)) …)

— *YeshuaAaron, 23 November 2012*

* * *

