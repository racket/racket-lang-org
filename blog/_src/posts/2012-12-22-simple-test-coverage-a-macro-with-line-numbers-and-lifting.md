
    Title:Simple Test Coverage: A Macro with Line Numbers and Lifting
    Date:2012-12-22T11:03:00.001-05:00
    Tags:

*posted by Robby Findler*

Racket's macro system makes it easy to roll your own low-tech line coverage tool. In this post, I'll show how, in 15 lines of code, you can implement a simple test-coverage tool. Using this code is simple: put `(line-of-interest)` on each line that should be covered.

To start the implementation, we put the code in a module and define two sets:

```racket
#lang racket
(define candidate-lines (set))
(define touched-lines (set))

```

The first set holds the line numbers where `(line-of-interest)` is written in the source and the second holds the set of line numbers where `(line-of-interest)` has been executed.

Each use of `(line-of-interest)` is going to expand into a call to visited with the line number for the source location of that use of `(line-of-interest)`.

```racket
(define (visited line)
  (unless (set-member? touched-lines line)
    (set! touched-lines (set-add touched-lines line))
    (displayln
     (sort (set->list
            (set-subtract candidate-lines touched-lines))
           <))))
```

This function simply checks to see if this line has been executed before and, if not, removes that line number from touched-lines and prints out the current status.

The interesting part of this code is in the definition of line-of-interest itself:

```racket
(define-syntax (line-of-interest stx)
  (with-syntax ([line (syntax-line stx)])
    (syntax-local-lift-expression
     #'(set! candidate-lines (set-add candidate-lines line)))
    #'(visited line)))
```

The macro first extracts the line number from stx, which gives the source location for the use of `(line-of-interest)`. This number is then bound to line for use in building later syntax objects. Then the macro calls syntax-local-lift-expression with a syntax object that updates candidate-lines. Expressions passed to syntax-local-lift-expression are lifted to the top-level of the enclosing module making sure that, in this case, each line number is added exactly once without having to execute the code where `(line-of-interest)` appears. The macro then discards the result of syntax-local-lift-expression and returns a call to the visited function. That's all there is to it!

I originally used this macro to test some changes to DrRacket. I was working on a set of complex GUI interactions and kept losing track of which ones had been tested and which ones hadn't. Here's a simpler program in the same spirit so you can try it out.

```racket
#lang racket/gui
(define candidate-lines (set))
(define touched-lines (set))
(define (visited line)
  (unless (set-member? touched-lines line)
    (set! touched-lines (set-add touched-lines line))
    (displayln
     (sort (set->list
            (set-subtract candidate-lines touched-lines))
           <))))
(define-syntax (line-of-interest stx)
  (with-syntax ([line (syntax-line stx)])
    (syntax-local-lift-expression
     #'(set! candidate-lines (set-add candidate-lines line)))
    #'(visited line)))
 
(define f (new frame% [label ""]))
 
(define b1 (new button%
                [label "1"]
                [parent f]
                [callback
                 (λ (a b)
                   (case (random 3)
                     [(0)
                      (line-of-interest)
                      (send b1 set-label "one")]
                     [(1)
                      (line-of-interest)
                      (send b1 set-label "uno")]
                     [(2)
                      (line-of-interest)
                      (send b1 set-label "一")]))]))
 
(define b2 (new button%
                [label "2"]
                [parent f]
                [callback
                 (λ (a b)
                   (case (random 3)
                     [(0)
                      (line-of-interest)
                      (send b2 set-label "two")]
                     [(1)
                      (line-of-interest)
                      (send b2 set-label "dos")]
                     [(2)
                      (line-of-interest)
                      (send b2 set-label "二")]))]))
(send f show #t)
```