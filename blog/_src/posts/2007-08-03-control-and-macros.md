
    Title:control and macros
    Date:2007-08-03T14:06:00.000-04:00
    Tags:

*posted by matthias*

After reading the posts on control operators, Vlado Zlatanov decided to
look into `prompt, control, fcontrol` and the rest of the
goodies in `control.ss`.

So based on the example from the blog post I did this python-like snippet: 

```racket
(define/y (step) 
  (yield 1)
  (yield 2)
  (yield 3)
  'finished)
```

He decided to look into turning it into a macro, such that the above
ends up being correct code. When he got stuck, he asked on our mailing list
and the resulting dialog  was so informative that I decided to blog it. 

My first replay was this suggestion: 

```racket
(define-syntax define/y
  (syntax-rules ()
    [(_ yield-name (name arg ...) body ...)
     (define (name arg ...)
       (define exit-with #f)
       (define (switch-control-context th)
         (call/cc 
          (lambda (k)
            (set! exit-with k)
            (th))))
       (define (yield-name x)
         (call/cc 
          (lambda (resume-here)
            (set! name 
               (lambda () 
                 (switch-control-context 
                  (lambda () 
                     (resume-here 'dummy)))))
            (exit-with x))))
       (switch-control-context (lambda () body ...)))]))
```

I sent this out with two suggestions. 

First, use `control.ss` to simplify the code. Second, use
`syntax-case` to eliminate the need for the programmer-user of
`define/y` to specify the name of `yield`. 

So, here is the prompt-based code: 

```racket
(require (lib "control.ss"))

(define-syntax define/y
  (syntax-rules ()
    [(_ yield-name (name arg ...) body ...)
     (define (name arg ...)
       (define (yield-name x)
         (control resume-here
            (set! name
                  (lambda ()
                    (prompt (resume-here 'dummy))))
            x))
       (prompt body ...))]))

(define/y yield (step) 
  (yield 1)
  (yield 2)
  (yield 3)
  'finished)

(equal? '(1 2 3) (list (step) (step) (step)))
```

This time I include a test case that assures the proper return behavior of
`yield`. The definition of `define/y` shows how to
mark the return point with `prompt` and how to switch to this
point with `control` so that your generator can resume the
traversal at the place where it was interrupted. 

For the second challenge, I wrote this definition: 

```racket
(require (lib "control.ss"))

(define-syntax (define/y stx)
  (syntax-case stx ()
    [(_ (name arg ...) body ...)
     (with-syntax 
         ((yield-name (datum->syntax-object stx 'yield)))
       (syntax
        (define (name arg ...)
          (define (yield-name x)
            (control resume-here
             (set! name 
                   (lambda ()
                     (prompt (resume-here 'dummy))))
             x))
          (prompt body ...))))]))

(define/y (step) 
  (yield 1)
  (yield 2)
  (yield 3)
  'finished)

(equal? '(1 2 3) (list (step) (step) (step)))
```

If you compare the two macro definitions, you notice very little
difference. Indeed, what really differs is the "interface" (the API), that
is, the way you can use the macro: see the test case. What also differs is
that the definition uses `syntax-case` and
`with-syntax` to inject `yield` into the body of
`define/y`.

In response, Vlado wrote "but isn't this non-hygienic." Here is my
response: 


Hygiene is a uniformity default imposed on the expander with a provision
for programmers to choose the non-default. I chose this word  carefully
when I coined the phrase. So what you have *is* a hygienic solution. 


In other words, injecting an identifier into a macro is not a violation of
hygiene at all. It's just means using the full power of the macro system.