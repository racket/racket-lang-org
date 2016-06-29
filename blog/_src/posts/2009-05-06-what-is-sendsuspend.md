
    Title:What is `send/suspend`?
    Date:2009-05-06T08:58:00.001-04:00
    Tags:

*posted by Jay McCarthy*

I often ponder what [`send/suspend`](http://docs.plt-scheme.org/web-server/servlet.html#(def._((lib._web-server/servlet/web..ss)._send/suspend))) really is. It is a lot like `call/cc`, but has the curious property that the continuation escapes in a single way and is only called in a particular context. I often wonder if there is something weaker than `call/cc` that implements `send/suspend`.

Today I wrote a little dispatcher that uses threads to implement `send/suspend`. In this implementation, _send_ing truly _suspend_s the computation.

Here's the code: [http://www.copypastecode.com/codes/view/5003](http://www.copypastecode.com/codes/view/5003)

The trick is to have channels for communicating responses and requests. When you run this example, you should be able to add two numbers. But, in contrast to the normal `send/suspend`, all the URLs are one-shots, because once the computation is resumed, it moves forward... it is never saved.

This implementation technique also precludes clever implementations of `send/suspend/dispatch`, like:

```racket
(define (send/suspend/dispatch mk-page)
  (let/cc k0
    (send/back
     (mk-page
      (lambda (handler)
        (let/ec k1 
          (k0 (handler (send/suspend k1)))))))))
```

<!-- more -->



* * *

As Shriram pointed out in email, he and I conjectured that there was an intimate relationship between s/s and call/cc in 2002. When I then gave my Oxford lecture, some German attendees (from Mike's or Peter's group, I believe) implemented call/cc via s/s. Question is what this really means.

— *matthias, 6 May 2009*

* * *

