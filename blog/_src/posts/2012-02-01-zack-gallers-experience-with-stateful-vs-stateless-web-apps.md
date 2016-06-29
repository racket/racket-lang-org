
    Title:Zack Galler's Experience with Stateful vs Stateless Web Apps
    Date:2012-02-01T19:25:00.000-05:00
    Tags:

*posted by Jay McCarthy*

Communication using HTTP between client and server is a simple problem of halted computation.
A client computes a request, transmits and halts, waiting for a server response.  On receipt, the server computes a response, transmits and halts, waiting for the next client request.
This much is well known.
Racket's magnificent _stateful_ Web server does three things on the server side:

* it reifies a Racket continuation, capturing where the server computation has halted.

* it externalizes the continuation, creating  a URL-representation that uniquely maps to the Racket continuation

* it disseminates the externalized continuation to interested clients, typically via HTTP response, but alternately via SMTP or any other protocol.
Then, it waits.
Later, when presented with an externalized continuation, a quick inverse mapping occurs, the underlying Racket continuation is invoked, and the server processes the new client request.
Rinse and repeat.
The problem with this approach is twofold

* the reified Racket continuations live in server memory.  And there's no safe way to garbage collect, as the continuations could be invoked at any time.  There are strategies to reclaim memory, but _some_ load level will noticeably decrease the performance of your application.  And its not possible to figure out what that load level is prior to finishing your application.  This is a problem.

* Again, the reified Racket continuations live in server memory and cannot be moved.  So there's no way to scale an application to more than one server.  It's a necessarily one machine system.  This makes problem #1 worse.
Racket's yet more magnificent _stateless_ Web server does exactly the same three things:

* to reify, it rewrites the entire call stack into a format known as [A-Normal Form](https://en.wikipedia.org/wiki/Administrative_normal_form) (ANF).

* to externalize, the ANF'd stack is encoded for transmission over HTTP.

* and then it's sent over to the client (dissemination).
Later, when presented with encoded stack, the stateless server performs an inverse transform to reconstruct the call stack, at which point the server keeps going.
So we've lost the invocation step and substituted a reconstruction.
But in exchange, we've eliminated continuations from server memory, and solved both enumerated problems above.  Neat trick.

I provide a few lessons learned for the archives for the next person to attempt porting `#lang racket` to `#lang web-server` code.
First, the predicate `serializable?` from `racket/serialize` is invaluable.  The `#lang web-server` code will not transform if there are non-serializable constructs in the dynamic extent of the invocation of `send/suspend`, such as a local binding or argument.
Second, invocations of native continuations reified with `call/cc` frequently throw errors related to continuation prompts, such as “attempt to cross a continuation barrier” or “no corresponding prompt tag in continuation”.  In all cases, I was able to remedy the situation by enclosing the invocation in `call-with-continuation-prompt`.  This may be an error in the system, but it is unclear at this time.
Third, the transformation does not allow parameters or `dynamic-wind`, because the internal data-structures representing them are not serializable, but continuation-marks can be used to reimplement the piece of the functionality you need.

Finally, thank you to the Racket team.  I think the stateless Web language is important technology and must have required an enormous amount of work to implement.
Anecdotally, application speed seems at or better than the stateful code.
To learn more about the stateless Web application infrastructure, [consult the manual](http://docs.racket-lang.org/web-server/stateless.html) or post to the [mailing list](http://lists.racket-lang.org/).
(This post was written by Zack Galler with minor edits before posting by Jay McCarthy.)

<!-- more -->



* * *

Please help me:

My stateless servlet allways throw exception:

```bash
D:\tmpracket\Racket\collects\web-server\dispatchers\dispatch-servlets.rkt:85:20: Servlet (@ /c.rkt;((%22c%22%20.%20%220((3)%202%20(((lib%20%5C%22web-server%2Flang%2Fabort-resume.rkt%5C%22)%20.%20%5C%22lifted.6%5C%22)%20((lib%20%5C%22web-server%2Flang%2Fweb-cells.rkt%5C%22)%20.%20deserialize-info:frame-v0))%200%20()%20()%20(0%20(1%20(h%20-%20()))%20()))%22))) exception:
set-servlet-handler!: contract violation, expected: can-be-response?, given: #
  contract from: 
    /web-server/private/servlet.rkt
  blaming: 
    /web-server/servlet/setup.rkt
  contract: 
    (->
     servlet?
     (-> request? can-be-response?)
     void?)
  at: /web-server/private/servlet.rkt:8.15
```



The stateless servlet:

```racket
(require web-server/http/xexpr
         web-server/managers/lru
         web-server/lang/web
         web-server/lang/stuff-url)

(define (start req)
  (define (render-home u)
    (response/xexpr
     `(body "Hello World: Stateless"
            (a ([href ,u]) " clickMe"))))
  (send/suspend render-home)

  (response/xexpr
   `(body "OK, You clickedMe!")))

(define interface-version 'stateless)

(define stuffer (make-default-stuffer (build-path "f:\\lisp\\scheme\\racket\\program\\urls.urls")))

(provide interface-version
         stuffer
         start)
```

My main dispatcher is :

```racket
#lang racket

(require web-server/web-server
         web-server/servlet-dispatch
         web-server/servlet/setup
         (prefix-in files: web-server/dispatchers/filesystem-map)
         (prefix-in servlets: web-server/dispatchers/dispatch-servlets))

(define p->s (make-default-path->servlet))
(define u->p (files:make-url->path "f:\\lisp\\scheme\\racket\\program\\"))

(define-values (_ u->s) (servlets:make-cached-url->servlet u->p p->s))
(define dis (servlets:make u->s))

;; (serve
;;  #:dispatch dis
;;  #:port 9979
;;  )


(serve/launch/wait
 (lambda (sem)
   dis)
 #:port 9968)
```

— *cnnzp, 9 March 2012*

* * *

@cnnzp, I copied exactly what you wrote in the comment (except I added #lang web-server) to the servlet and everything worked fine. If you'd like to work through it more, I suggest you email the mailing list, as it is a better place to get support.

Jay

— *Jay McCarthy, 12 March 2012*

* * *

Interesting and useful... my initial wondering though is how easy would it be to forge an interesting continuation? I can think of ways to guard against that as needed.

— *patrickdlogan, 7 August 2012*

* * *

@patrickdlogan, It is pretty easy to change the free variables, but basically impossible to change the control flow. There's a trivial option in the Web server to enable signing (so that you can detect forgery) or encryption (so that you can disable their ability to read the free vars)

— *Jay McCarthy, 8 August 2012*

* * *

