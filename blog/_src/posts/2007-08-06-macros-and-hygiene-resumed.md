
    Title:macros and hygiene, resumed
    Date:2007-08-06T11:21:00.000-04:00
    Tags:

*posted by matthias*

The Friday entry demonstrates how to break hygiene for a macro that defines a generator. Ryan Culpepper, the local macrologist, reminded me that expanding into this macro goes wrong in the syntax-case world: 

```racket
(define-syntax define-that-expands-into-define/y 
  (syntax-rules ()
    ((_ (name arg ...) body ...) 
     (define/y (name arg ...) body ...))))

(define-that-expands-into-define/y (bar)
  (yield 1)
  (yield 2)
  'finished)
```

Run this in Pretty Big [DrScheme] and you get a strange note concerning MrEd's yield or run it in MzScheme [Textual] and you get an error message about 'yield' being unbound. 

What gives? The 'stx' of `datum->syntax-object` is the syntactic context of the new macro but it doesn't bind yield; it just uses it. So the definition of `yield` in `define/y` must be a different one according to the hygiene standards. Ergo yield is free at the top-level [MzScheme] or bound to the yield import from MrEd [Pretty Big]. 

How can we try to fix this? The explanation suggests we use a different macro definition for `define/y`, one that uses a context that is guaranteed from the body of an instance of `define/y`: 

```racket
(require (lib "control.ss"))

(define-syntax (define/y stx)
  (syntax-case stx ()
    [(_ (name arg ...) body0 body ...)
     (with-syntax 
         ((yield-name 
           (datum->syntax-object (syntax body0) 'yield)))
       (syntax
        (define (name arg ...)
          (define (yield-name x)
            (control resume-here
             (set! name 
                   (lambda ()
                     (prompt (resume-here 'dummy))))
             x))
          (prompt body0 body ...))))]))

(define-syntax define-that-expands-into-define/y 
  (syntax-rules ()
    ((_ (name arg ...) body ...) 
     (define/y (name arg ...) body ...))))

;; --- try it out ---

(define-that-expands-into-define/y (bar)
  (yield 1)
  (yield 2)
  'finished)

(list (bar) (bar) (bar) (bar))
```

Run it. You will find that it works as expected. 

Tomorrow, time permitting, I will tell you what's wrong with it and how you can fix it.

<!-- more -->



* * *

Isn't it still a problem that we cannot use yield in macros?

As a silly example:

```racket
(define-syntax yield2
  (syntax-rules (yield)
    ((_ x) (yield x))))

(define/y (step)
 (yield2 1)
 'finished))
```

We'll get a yield unbound error when calling step.

— *Léa, 22 August 2007*

* * *

This comment has been removed by the author.

— *Léa, 22 August 2007*

* * *

Ah, OK, this has to do with the lexical scope of yield.

If I define

```racket
(define/y (step)
  (define-syntax yield2
  (syntax-rules ()
    ((_ x) (yield x))))
  (yield2 1)
  'finished)
```

instead, it works.

— *Léa, 22 August 2007*

* * *

