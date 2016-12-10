
    Title:`call/cc` and self-modifying code
    Date:2007-07-27T18:45:00.000-04:00
    Tags:

*posted by matthias*

Today I wrote this short illustration of call/cc and posted it on wikipedia: 

```racket
;; [LISTOF X] -> ( -> X u 'you-fell-off-the-end-off-the-list)
(define (generate-one-element-at-a-time a-list)
  ;; (-> X u 'you-fell-off-the-end-off-the-list)
  ;; this is the actual generator, producing one item from a-list at a time
  (define (generator)
     (call/cc control-state)) 
  ;; [CONTINUATION X] -> EMPTY
  ;; hand the next item from a-list to "return" (or an end-of-list marker)'
  (define (control-state return)
     (for-each 
        (lambda (an-element-from-a-list)
           (set! return ;; fixed
             (call/cc
               (lambda (resume-here)
                 (set! control-state resume-here)
                 (return an-element-from-a-list)))))
        a-list)
     (return 'you-fell-off-the-end-off-the-list))
  ;; time to return the generator
  generator)
```

It reminded of all the talk in the 1980s and 1990s that self-modifying code is bad. But look at the elegant assignment to `control-state` within its body. It's such a poem, I thought I'd share it with people since nobody else blogs here anywya.

<!-- more -->



* * *

This is really beautiful. :)

— *Jianshi Huang, 28 July 2007*

* * *

A pretty little ditty, to be sure. Much of object-oriented programming relies on self-modifying code, if you think about it. Consider the strategy pattern or the state pattern. In object-oriented programs it may appear that we are changing the data, but we are really changing the code.

— *I, Object, 28 July 2007*

* * *

Hello, a quick question:

I'm not a scheme guru, but the posted code seems a bit off. Specifically, it looks to me like 'return' is only bound the first time generator is called, so if you used it like:

   (generator)
   ...
   (generator)

On the second call, it would jump back to the first call (since that's what the continuation stored in 'return' is). It seems like it should instead be something like (totally untested):

  ...
  (lambda (an-element-from-a-list)
     (set! return
           (call/cc
               (lambda (resume-here)
                   (set! control-state resume-here)
                   (return an-element-from-a-list)))))
  ...

However, as I said, I don't write a lot of scheme, so maybe I'm missing something.

Apologies for the terrible formatting, but I'm not sure how to tell blogger to display stuff as code.

Cheers.

— *Dan Doel, 28 July 2007*

* * *

this is a terrible misuse of call/cc.
here is a better implementation:

(define (one-at-a-time lst)
  (lambda ()
    (if (null? lst)
 'you-fell-off-the-end
 (let ((x (car lst)))
   (set! lst (cdr lst))
   x))))

— *Gavin, 29 July 2007*

* * *

@Gavin: Actually, it is exactly what the recent call/cc hype was about: To allow implicit (foreach) instead of explicit state machines, esp. in the context of web applications. Imagine the state getting a little bit more complicated. (You'd need to hide the call/cc in an abstraction for the generator programmer as well, though.)

— *Andreas Krey, 29 July 2007*

* * *

call/cc is indeed a bad choice if all you're doing is making a generator out of a flat list. However, the technique mentioned works about equally well for turning folds over arbitrary data structures into generators/cursors.

For instance, writing a generator for a binary tree using only the analogues to car/cdr is a pain (I imagine you'd manage a stack of nodes to be visited by hand). However, writing an in-order traversal is simple. This example shows that you can turn the latter into the former automatically and generically using call/cc.

Of course, for the particular case of turning a fold into a cursor, delimited continuations are, perhaps, even nicer (and, in a sense, this example might be mimicking delimited continuations with call/cc + state). Something like:

(define (mk-generator traversal structure)
   (define (generator) (control-loop))

   (define (control-loop)
      (reset
         (traversal
            (lambda (element)
               (shift
                  (lambda (k)
                     (set! control-loop (lambda () (k #f)))
                     element)))
            structure)
         'traversal-done))

   generator)

Which, handily, avoids having to worry about keeping track of a return continuation for this problem.

The problem of turning folds into cursors in various languages, and the advantages of structuring a collection library in such a way are discussed in more depth here.

— *Dan Doel, 29 July 2007*

* * *

1. Thanks for pointing out the bug. 

2. This (correct) code is a  poem on turning a loop into a generator. It is not serious code. 

3. And yes, it is writing code like that many times, which first suggested delimited continuations to me in the fall of 1984; it just took three years to write it up.

— *matthias, 29 July 2007*

* * *

Matthias, sorry to be commenting on such an old blog post ;-)

This is a pretty cool example, but I think it would be clearer if the internal defines were desugared into a letrec.  It's very odd to see a function definition mutated from inside the function in the first place, but somehow it seems more natural in a letrec.  More importantly (for me at least), is that I can understand right away what the continuation of the letrec would be, but it's not at all obvious with the internal define -- it looks as though the continuation of the (define (generator) ...) code would continue by defining control-state again.

— *Michael, 30 January 2008*

* * *

This is cute; it took me a minute to understand what was going on.   On the other hand,  a few years ago I would have been scratching my head for hours.

One complaint though:  this code holds onto a return continuation longer than necessary, introducing the possibility of memory leaks and/or poor GC performance.

Is there some kind of generic technique for destroying a reference to a continuation "on the way out" as you call it?

— *Leon Smith, 3 April 2008*

* * *

