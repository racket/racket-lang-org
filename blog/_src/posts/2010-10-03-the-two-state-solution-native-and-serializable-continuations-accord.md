
    Title:The Two-State Solution: Native and Serializable Continuations Accord
    Date:2010-10-03T00:57:00.000-04:00
    Tags:

*posted by Jay McCarthy*

The Racket Web Server allows an expressive way of writing Web applications using first-class continuations to capture the control-flow of the server while it is waiting for the client to respond. For example:

```racket
#lang web-server/insta
(define (get-number p)
  (string->number
   (extract-binding/single
    'num
    (request-bindings
     (send/suspend
      (λ (k-url)
        `(html 
          (body
           (form ([action ,k-url])
                 ,p nbsp (input ([name "num"]))
                 (input ([type "submit"])))))))))))
(define (start req)
  (define how-many
    (get-number "How many numbers to add?"))
  (number->string
   (foldr 
    + 0
    (build-list 
     how-many
     (λ (i)
       (get-number 
        (format "Provide number: ~a" 
                (add1 i))))))))
```

This application creates a re-usable `get-number` interaction abstraction and uses it in a number of different contexts. In particular, it uses it in the higher-order context of `build-list`. This application also reuses useful third-party library functions like `foldr`, etc.

Such an application would be complicated to write in a traditional Web programming environment because the continuation of each `get-number` invocation is considerably more complex than is typical. Yet, the first-class continuations in Racket ensure that this continuation is captured exactly, correctly, every time.

Unfortunately, the native first-class continuations of Racket are not serializable, so they impose a per-session resource expenditure on the server. This can be alleviated through [expiration policies](http://docs.racket-lang.org/web-server/servlet.html#(part._managers)), but such policies are inherently unsound because continuations URLs are global roots.

In the past, PLT has [provided tools](http://cs.brown.edu/~sk/Publications/Papers/Published/mfgkf-web-restructuring-cps-journal/) that automatically restructure this kind of program into one that uses serializable continuations through an acronym soup of source transformations: CPS, lambda-lifting, defunctionalization, SPS, and so on. These tools effectively create automatically what most Web programmers write manually, except the tools don't mistakes.  But the tools also don't take into consideration what functions actually contribute to the interaction context and transform library functions like `foldr` (which is unnecessary in the continuation) the same as functions like `build-list` (which _are_ necessary.)

Our [past work](http://faculty.cs.byu.edu/~jay/static/icfp065-mccarthy.pdf) (based on [another PLT paper](http://cs.brown.edu/~sk/Publications/Papers/Published/pcmkf-cont-from-gen-stack-insp/)) alleviates this problem by only requiring functions like `build-list` to be transformed. From the perspective of a programmer, "transformed" is tantamount to "rewritten" because the source code for a third-party library may not be readily available. Programmers would have to program `add-many-numbers.com` as:

```racket
#lang web-server
(require web-server/servlet-env)
(define (get-number p)
  (string->number
   (extract-binding/single
    'num
    (request-bindings
     (send/suspend
      (λ (k-url)
        `(html 
          (body
           (form ([action ,k-url])
                 ,p nbsp (input ([name "num"]))
                 (input ([type "submit"])))))))))))
(define (build-list n f)
  (for/list ([i (in-range n)])
    (f i)))
(define (start req)
  (define how-many
    (get-number "How many numbers to add?"))
  (number->string
   (foldr 
    + 0
    (build-list
     how-many
     (λ (i)
       (get-number 
        (format "Provide number: ~a"
                (add1 i))))))))
; This requires a pre-release version
; to run in an un-named DrRacket buffer
(serve/servlet start #:stateless? #t)
```


where `build-list` has been re-implemented, but functions like `foldr` have not. This application, despite its striking similarity to the first, requires absolutely no per-session server state, so it is considerably more scalable.

Do we need to re-implement `build-list`? What if the third-party, higher-order function (`build-list`) that we use with a higher-order argument that causes Web interaction (`get-number`) is too complicated to re-implement?
 
Naturally this blog post would not exist if we didn't solve this problem.

Our new approach, dubbed The Two-State Solution, allows the programmer to transparently use a very small amount of per-session server state to store _just_ the part of the continuation inside functions like `build-list` while serializing everything else to the client.

The key is to use [delimited, composable continuations](http://docs.racket-lang.org/reference/eval-model.html#(part._prompt-model)) to isolate the appropriate part of the continuation. The programmer designates this piece of the continuation through the `serial->native` and `native->serial` annotations. The programmer can write the application as:

```racket
#lang web-server
(require web-server/servlet-env)
(define (get-number p)
  (string->number
   (extract-binding/single
    'num
    (request-bindings
     (send/suspend
      (λ (k-url)
        `(html 
          (body
           (form ([action ,k-url])
                 ,p nbsp (input ([name "num"]))
                 (input ([type "submit"])))))))))))
(define (start req)
  (define how-many
    (get-number "How many numbers to add?"))
  (number->string
   (foldr 
    + 0
    (serial->native
     (build-list
      how-many
      (λ (i)
        (native->serial
         (get-number 
          (format "Provide number: ~a"
                  (add1 i))))))))))
; This requires a pre-release version
; to run in an un-named DrRacket buffer
(serve/servlet start #:stateless? #t)
```


The important distinction here is that both the `build-list` and the `get-number` abstractions do not need to change. We simply mark the context as being a "serial" or "native" context through the annotation forms. This re-written version will be more scalable than a purely native version, but represents an easier to achieve step in the evolution of a program, because third-party, higher-order functions can be used as is.

This work will be presented at [OOPSLA 2010](http://splashcon.org/index.php?option=com_content&amp;view=article&amp;id=122&amp;Itemid=91). It is also described in a paper with same name this blog post:[The Two-State Solution: Native and Serializable Continuations Accord](http://faculty.cs.byu.edu/~jay/static/oopsla026-mccarthy.pdf).

<!-- more -->



* * *

Awesome sauce. Can't wait to try it out!

I came across racket a couple of weeks ago when a link to your site came up on HN.

This is some seriously great kit you've put together. I'm having a lot of fun with this language. Coming from CL, it's actually a rather pleasant transition. Definitely a step-up from the Python/C++ I do at my day job.

Keep up the good work! :)

— *j_king, 2 November 2010*

* * *

