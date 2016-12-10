
    Title:Soft State in the PLT Web Server
    Date:2009-05-18T16:47:00.000-04:00
    Tags:

*posted by Jay McCarthy*

Many Web applications and network protocols have values in the continuation that are necessary to complete the computation, but that may be recomputed if they are not available. This is "soft state".

For example, a Web application may cache a user's preferences from a database and deliver it to a Web browser as a hidden value; when the value is returned to the application in subsequent steps, it is used to customize the display. However, if the preferences were not available (or were corrupted in some way), the application could retrieve them from the database.

When using the PLT Web Server's native continuations, this roughly corresponds to the use of a _weak box_: a box that the GC is allowed to erase the contents of. When using the PLT Web Server's serializable continuations it roughly corresponds to a weak box and a _weak hash table_ (that holds its keys weakly) to give the box a serializable value as an identifier.

This programming pattern is a bit difficult to get right. So, a library that implements it is now provided with PLT Scheme: [`web-server/lang/soft`](http://faculty.cs.byu.edu/~jay/plt-doc/web-server/stateless.html#(part._.Soft_.State)).

Here's a trivial example:

```racket
#lang web-server

(provide interface-version start)
(define interface-version 'stateless)

(define softie
  (soft-state
   (printf "Doing a long computation...~n")
   (sleep 1)))

(define (start req)
  (soft-state-ref softie)
  (printf "Done~n")
  (start
   (send/suspend
    (lambda (k-url)
      `(html (body (a ([href ,k-url]) "Done")))))))
```
